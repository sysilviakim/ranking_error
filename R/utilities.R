# Library and function calls ===================================================
library(plyr)
library(MASS)
library(here)
library(combinat) # For permn
library(gtools) # For permute
library(tidyverse)
library(styler)
library(PLMIX) # For switch_ord_rank()
library(Kmisc)
library(patchwork)
library(janitor)
library(assertthat)
library(pwr)

# READING THE FUNCTION THAT DRAW FROM PLACKETT-LUCE
source(here("R", "rpluce.R"))

# Parameters ===================================================================
bootstrap_n <- 1000

# Functions ====================================================================
## Turn 1st, 2nd, and 3rd ranking columns into long data format
pivot_sim <- function(x) {
  out <- x %>%
    pivot_longer(
      cols = c(V1, V2, V3), names_to = "position", values_to = "item"
    ) %>%
    mutate(
      rank = case_when(
        position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
        position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
        position == "V3" ~ str_sub(obs_rank, 3, 3) # Third value
      )
    ) %>%
    arrange(id, rank)
  return(out)
}

loop_stated_rank_preference <- function(true_rank, choice) {
  # Storage for observed patterns
  obs_pattern <- data.frame(
    pattern1 = integer(),
    pattern2 = integer(),
    pattern3 = integer()
  )

  # For each i-th unit in true_rank
  for (i in 1:dim(true_rank)[1]) {
    # Cast numbers to alphabets
    vec_pref <- true_rank[i, ] %>%
      pivot_longer(cols = c(a, b, c), names_to = "variable")
    vec_pref # Check

    # Alphabet unit i sees in each position
    position1 <- choice[i, 1] %>% pull()
    position2 <- choice[i, 2] %>% pull()
    position3 <- choice[i, 3] %>% pull()

    # Assign a value (rank) for each position
    # given the alphabet unit i sees there
    pattern1 <- vec_pref$value[vec_pref$variable == position1]
    pattern2 <- vec_pref$value[vec_pref$variable == position2]
    pattern3 <- vec_pref$value[vec_pref$variable == position3]

    # Combine the result
    comb <- data.frame(
      pattern1 = pattern1,
      pattern2 = pattern2,
      pattern3 = pattern3
    )

    # Stack in the storage
    obs_pattern <- rbind(obs_pattern, comb)

    # End of i loop
  }

  obs_pattern <- obs_pattern %>% unite(obs_rank, sep = "")
  return(as_tibble(obs_pattern))
}

prop_vector <- function(x, digits = 1) {
  return(round(prop.table(table(x, useNA = "ifany")), digits = digits))
}

rowid <- function(x) {
  out <- x %>%
    mutate(id = row_number()) %>%
    select(id, everything())
  return(out)
}

pivot_join <- function(x, y) {
  return(pivot_sim(left_join(y, x)) %>% select(id, obs_rank, item, rank))
}

## Recover the reference (true) ranking
## with respect to the reference item set (here: {abc})
recov_ref_ranking <- function(dat, rank_var = "obs") {
  # For each i-th unit in data
  ref_data <- seq(nrow(dat)) %>%
    map(
      ~ {
        temp <- dat[.x, ] %>%
          separate(
            !!as.name(rank_var),
            sep = c(1, 2), into = c("first", "second", "third")
          ) %>%
          ## V1, V2, and V3 are randomized choice order given to respondent
          select(first, second, third, contains("V")) %>%
          pivot_longer(cols = c(V1, V2, V3), names_to = "variable")

        ## Recovering the true ranking given the reference set (abc)
        ## case_when is faster outside a pipe
        temp$recover <- case_when(
          temp$variable == "V1" ~ temp$first,
          temp$variable == "V2" ~ temp$second,
          temp$variable == "V3" ~ temp$third
        )

        temp <- temp %>%
          arrange(value) %>%
          .$recover %>%
          ## ref_ranking, i.e., concatenated true rankings
          paste(collapse = "")
        return(temp)
      }
    ) %>%
    ## combine
    unlist() %>%
    tibble(ref = .)
  return(ref_data)
}

## Plot the distribution of observed rankings (over permutation space)
plot_dist_ranking <- function(x, J = 3, ylim = 0.3) {
  x %>%
    ggplot(aes(x = ranking, y = prop, fill = "1")) +
    geom_col() +
    scale_fill_manual(values = "firebrick4") +
    xlab("Observed Ranking") +
    ylab("") +
    scale_y_continuous(labels = scales::percent, limits = c(0, ylim)) +
    geom_hline(yintercept = 1 / factorial(J)) +
    geom_text(
      aes(
        label = paste0(round(prop * 100, digits = 1), "%"),
        family = "CM Roman"
      ),
      vjust = -0.5, size = 3
    )
}

## Print effect size and power
chisq_power <- function(tab, power = 0.95) {
  ## Chi-square test
  message("The chi-square test result is:")
  print(chisq.test(tab, p = rep(1 / length(tab), length(tab))))

  ## Power test. We could do the power = 0.8, sure
  P0 <- rep(1 / length(tab), length(tab))
  P1 <- as.numeric(prop.table(tab))
  message(paste0("Effect size is ", ES.w1(P0, P1)))
  message("The chi-square power test result is:")
  print(
    pwr.chisq.test(w = ES.w1(P0, P1), df = (length(tab) - 1), power = power)
  )
}

## Turn ranking dist. summary table to a tibble
table_to_tibble <- function(tab) {
  enframe(tab, name = "ranking", value = "freq") %>%
    mutate(
      ranking = factor(ranking),
      freq = as.numeric(freq),
      prop = freq / sum(freq)
    )
}

## If necessary, supplement all possible permutation patterns
## for a distribution table
permn_augment <- function(tab, J = 4) {
  deframe(
    enframe(tab) %>%
      bind_rows(
        ., tibble(name = permn(seq(J)) %>%
          map(~ paste(.x, collapse = "")) %>%
          unlist() %>%
          setdiff(., names(tab)), value = as.table(0))
      )
  )
}

# Text options wrangle to number ===============================================
text_to_item_position <- function(x) {
  ## For no-context questions, the presented order doesn't matter --------------
  x <- x %>%
    select(
      -contains("no_context_3_opts_1_do"),
      -contains("no_context_4_opts_1_do"),
      -contains("no_context_4_opts_2_do")
    )

  ## Identity ------------------------------------------------------------------
  x <- x %>%
    mutate(
      across(
        contains("identity"),
        ~ gsub(
          "Gender", "1",
          gsub(
            "City", "2",
            gsub(
              "Country", "3",
              gsub(
                "Socioeconomic Status", "4",
                gsub(
                  "Racial or Ethnic Group", "5",
                  gsub(
                    "Political Party", "6",
                    gsub("Religion", "7", gsub("\\|", "", .x))
                  )
                )
              )
            )
          )
        ),
      )
    )

  ## Nelson (1997) -------------------------------------------------------------
  x <- x %>%
    mutate(
      across(
        contains("nelson1997"),
        ~ gsub(
          paste0(
            "Print media|",
            "A person's freedom to speak and hear what he or she wants should be protected"
          ),
          "1",
          gsub(
            paste0(
              "Television|",
              "Campus and workplace safety and security should be protected"
            ),
            "2",
            gsub(
              paste0(
                "The Internet|",
                "School or workplace's reputation should be protected"
              ),
              "3",
              gsub(
                paste0(
                  "Social media|Social Media|", ## corrected capitalization
                  "Racism and prejudice should be opposed"
                ),
                "4",
                gsub("\\|", "", .x)
              )
            )
          )
        )
      )
    )

  ## Voting Mode ---------------------------------------------------------------
  x <- x %>%
    mutate(
      across(
        contains("voting"),
        ~ gsub(
          "Primary elections|Vote in-person on Election Day",
          "1",
          gsub(
            paste0(
              "Nomination of primary winners as candidates in the general election|",
              "Vote in-person during the early voting period"
            ),
            "2",
            gsub(
              paste0(
                "Early Voting starts \\(Before Election Day\\)|",
                "Send a mail ballot by post"
              ),
              "3",
              gsub(
                paste0(
                  "Election Day \\(November 5, 2024\\)|",
                  "Drop a mail ballot at a dropbox or polling place"
                ),
                "4",
                gsub(
                  paste0(
                    "Winner of general election sworn in as the next president|",
                    "Not vote"
                  ),
                  "5",
                  gsub("\\|", "", .x)
                )
              )
            )
          )
        )
      )
    )

  ## Partisanship --------------------------------------------------------------
  x <- x %>%
    mutate(
      across(
        contains("party_id_3_cands"),
        ~ gsub(
          "Registered Democrat", "1",
          gsub(
            "Registered Republican", "2",
            gsub("Independent", "3", gsub("\\|", "", .x))
          )
        )
      )
    )

  x <- x %>%
    mutate(
      across(
        contains("party_id_4_cands"),
        ~ gsub(
          "Registered Democrat 1", "1",
          gsub(
            "Registered Democrat 2", "2",
            gsub(
              "Registered Republican 1", "3",
              gsub("Registered Republican 2", "4", gsub("\\|", "", .x))
            )
          )
        )
      )
    )

  ## Symbols -------------------------------------------------------------------
  x <- x %>%
    mutate(
      across(
        contains("symbol"),
        ~ gsub(
          "△", "1",
          gsub(
            "□", "2",
            gsub(
              "⬠", "3",
              gsub("⬡", "4", gsub("\\|", "", .x))
            )
          )
        )
      )
    )

  ## Tate (1993) ---------------------------------------------------------------
  x <- x %>%
    mutate(
      across(
        contains("tate1993"),
        ~ gsub(
          paste0(
            "Federal government that create policies that affect people's lives at the federal level|",
            "Working in Congress on bills concerning national issues"
          ),
          "1",
          gsub(
            paste0(
              "State government that create policies that affect people's lives at the state level|",
              "Helping people in the district who have personal problems with government"
            ),
            "2",
            gsub(
              paste0(
                "Municipal government that create policies that affect people's lives at the city level|",
                "Making sure the state/district gets its fair share of government money and projects"
              ),
              "3",
              gsub("\\|", "", .x)
            )
          )
        )
      )
    )

  return(x)
}

## Collapse into resulting ranking pattern in the permutation space
unite_ranking <- function(x) {
  x_raw <- x

  ## Find all patterns and bring it to its "root," i.e., without the _1
  var_list <- x %>%
    select(ends_with("_1")) %>%
    names() %>%
    str_sub(end = -3)

  ## WHY are there five options yet Qualtrics decides to skip 5 and go to 6??
  if ("app_voting_6" %in% names(x)) {
    x <- x %>%
      rename(app_voting_5 = app_voting_6)
  }

  x <- x[, sort(names(x))]

  ## Perform for each pattern
  for (v in var_list) {
    ## Is it 3-option or 4-option?
    l <- x %>%
      select(contains(v)) %>%
      select(-ends_with("_do")) %>%
      ncol()

    ## Shows how important variable naming is in Qualtrics :)
    if (v == "symbols_3_opts") {
      l <- 3
    } else if (v == "symbols_4_opts") {
      l <- 4
    }

    x <- x %>%
      mutate(
        across(
          !!as.name(paste0(v, "_1")):!!as.name(paste0(v, "_", l)),
          ~ case_when(.x == "-99" ~ "9", TRUE ~ .x)
        )
      ) %>%
      unite(
        !!as.name(v),
        sep = "", !!as.name(paste0(v, "_1")):!!as.name(paste0(v, "_", l))
      )
  }

  ## restructure variable order
  x <- x %>%
    select(
      names(x_raw %>% select(start_date:berinsky_screener)),
      ## Lucid generated
      rid, age_2, gender_2, hhi, ethnicity, hispanic, education_2,
      political_party, region, zip,
      everything()
    )

  return(x)
}

## Avg. rank compute 
avg_rank <- function(df, var) {
  l <- nchar(df[[var]][[1]])
  v <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th")
  out <- df %>%
    select(!!as.name(var)) %>%
    separate(!!as.name(var), sep = seq(9)[1:l], into = v[1:l]) %>%
    summarise(across(everything(), ~ mean(as.numeric(.x))))
  return(out)
}

avg_rank_bootstrap_quantile <- function(x) {
  if (class(x) != "list") {
    stop("Input is not a list.")
  }
  x %>%
    imap(
      ~ .x %>%
        pivot_longer(everything(), names_to = "variable") %>%
        group_by(variable) %>%
        summarize(
          mean_val = mean(value),
          low = quantile(value, probs = 0.025),
          up = quantile(value, probs = 0.975)
        ) %>%
        mutate(Estimator = .y)
    ) %>%
    bind_rows() %>%
    mutate(
      Estimator = case_when(
        Estimator == "debiased" ~ "De-biased",
        TRUE ~ "Naive"
      )
    )
}

cor_and_condprob <- function(df, v1, v2) {
  ## Something like with(df, table(tate = v1, identity = v2)) fails
  ## Not much point in printing table, I guess...
  print(cor(df[[v1]], df[[v2]]))
  pretty_condprob(df, v1, 1, v2, 1)
  pretty_condprob(df, v2, 1, v1, 1)
}
