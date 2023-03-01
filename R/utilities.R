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

# Parameters/stored vectors ====================================================
bootstrap_n <- 1000
root_var <- c(
  tate1993 = "123", identity = "1234567", nelson1997 = "1234", voting = "12345"
)
## Previously...
# x <- c(
#   `1` = "Gender", `2` = "City", `3` = "Country", `4` = "Socioeconomic Status",
#   `5` = "Racial or Ethnic Group", `6` = "Political Party", `7` = "Religion"
# )
# match(x, sort(x)) %>% paste(collapse = "") ## 3127546

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

## import and wrangle Qualtrics data
qualtrics_import <- function(fname) {
  df_raw <- read_csv(here("data", "raw", fname), show_col_types = FALSE) %>%
    clean_names() %>%
    filter(
      start_date != "Start Date" &
        start_date != '{"ImportId":"startDate","timeZone":"America/Denver"}'
    ) %>%
    mutate(
      duration_in_seconds = as.numeric(duration_in_seconds),
      q_recaptcha_score = as.numeric(q_recaptcha_score)
    ) %>%
    rename(rcv_support = rc_vsupport_1)

  ## Status needs to be "IP Address" which is 0.
  temp <- df_raw %>%
    filter(status == "0")

  ## If IP addresses overlap, that's a major red flag so stop
  ## assert_that(!any(duplicated(main$ip_address)))
  if (any(duplicated(temp$ip_address))) {
    stop("There are duplicates in IP addresses. Please investigate.")
  }

  message(paste0("We have total ", nrow(temp), " respondents."))

  ## Separate out timing variables to actual responses
  timing <- temp %>%
    select(response_id, contains("timing"), -contains("time"))

  main <- temp %>%
    select(-contains("timing"), -contains("time"))

  x <- sum(main$q_recaptcha_score < 0.8, na.rm = TRUE) +
    sum(is.na(main$q_recaptcha_score))
  message(
    x, " out of ", nrow(main), ", or ",
    paste0(
      round(x / nrow(main) * 100, digits = 1),
      "% of rows have recaptcha score less than 0.8."
    )
  )
  main <- main %>%
    ## Filter out observations with recaptcha score < 0.8
    filter(q_recaptcha_score >= 0.8) %>%
    mutate(across(ends_with("_do"), ~ gsub("\\|", "", .x))) %>%
    unite_ranking()

  ## Create binary indicators for partial rankers + attention check fails
  main <- main %>%
    ## For nonsincerity + attention check fails
    mutate(
      ternovsky_fail = case_when(ternovsky_screener2 != "1,2" ~ 1, TRUE ~ 0),
      berinsky_fail = case_when(berinsky_screener != "4,12" ~ 1, TRUE ~ 0),
      ## nonsincere: weak context symbols
      ns_symbol_3opts = case_when(symbols_3_opts_known == "123" ~ 1, TRUE ~ 0),
      ns_symbol_4opts = case_when(symbols_4_opts_known == "1234" ~ 1, TRUE ~ 0),
      ## nonsincere: weak context party ID
      pid3 = case_when(
        pid7 %in% c("1", "2", "3") ~ "Dem",
        pid7 %in% c("5") ~ "Ind",
        pid7 %in% c("6", "7", "8") ~ "Rep",
        TRUE ~ "Not sure"
      ),
      ## Not easy to code. For now, allow "not sure" to be entirely sincere
      ns_party_3cand = case_when(
        ## If Dem, Dem - Ind - Rep, and if Rep, Rep - Ind - Dem
        pid3 == "Dem" & party_id_3_cands == "132" ~ 0,
        pid3 == "Rep" & party_id_3_cands == "231" ~ 0,
        pid3 == "Ind" & substr(party_id_3_cands, 1, 1) == "3" ~ 0,
        pid3 == "Not sure" ~ 0,
        TRUE ~ 1
      ),
      ## For 2-party context, allow "independent" and "not sure" to be sincere
      ns_party_4cand = case_when(
        ## If Dem, Dem - Ind - Rep
        pid3 == "Dem" & 
          party_id_4_cands %in% c("1234", "2134", "1243", "2143") ~ 0,
        pid3 == "Rep" & 
          party_id_4_cands %in% c("3412", "3421", "4312", "4321") ~ 0,
        pid3 == "Ind" | pid3 == "Not sure" ~ 0,
        TRUE ~ 1
      ),
      ## nonsincere: applications
      ns_tate = case_when(anc_tate1993 != "123" ~ 1, TRUE ~ 0),
      ns_nelson = case_when(anc_nelson1997 != "1234" ~ 1, TRUE ~ 0),
      ns_identity = case_when(anc_identity != "1234567" ~ 1, TRUE ~ 0),
      ns_voting = case_when(anc_voting != "12345" ~ 1, TRUE ~ 0)
    ) %>%
    ## Partial rankers
    mutate(
      partial_tate_main = case_when(grepl("9", app_tate1993) ~ 1, TRUE ~ 0),
      partial_tate_anc = case_when(grepl("9", anc_tate1993) ~ 1, TRUE ~ 0),
      partial_nelson_main = case_when(grepl("9", app_nelson1997) ~ 1, TRUE ~ 0),
      partial_nelson_anc = case_when(grepl("9", anc_nelson1997) ~ 1, TRUE ~ 0),
      partial_identity_main = case_when(grepl("9", app_identity) ~ 1, TRUE ~ 0),
      partial_identity_anc = case_when(grepl("9", anc_identity) ~ 1, TRUE ~ 0),
      partial_voting_main = case_when(grepl("9", app_voting) ~ 1, TRUE ~ 0),
      partial_voting_anc = case_when(grepl("9", anc_voting) ~ 1, TRUE ~ 0)
    ) %>%
    ## Transitivity (only relevant in pretest 1: 35% violated)
    ## pretest 2 has only given respondent one of 3 or 4 symbols question
    ## This is available in the symbols question. 
    ## For example, if they said they prefer triangle > square > pentagon,
    ## the addition of hexagon as an option should not reverse square > triangle
    ## So that if the answer to symbols_3_opts was 321, with the addition of 4,
    ## we should only allow 4321, 3421, 3241, or 3214. 
    rowwise() %>%
    mutate(
      transitivity_violate = case_when(
        is.na(symbols_4_opts) ~ NA_real_,
        symbols_4_opts %in% transitivity_pattern(symbols_3_opts) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    ungroup()

  return(list(main = main, timing = timing, raw = df_raw))
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
plot_dist_ranking <- function(x, ylim = 0.315) {
  J <- nchar(as.character(x$ranking[[1]]))
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

## Collapse into resulting ranking pattern in the permutation space
unite_ranking <- function(x) {
  x_raw <- x

  ## Find all patterns and bring it to its "root," i.e., without the _1
  var_list <- x %>%
    select(ends_with("_1")) %>%
    names() %>%
    str_sub(end = -3)

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
      ) %>%
      mutate(
        !!as.name(v) := case_when(
          grepl("NANA", !!as.name(v)) ~ NA_character_,
          TRUE ~ !!as.name(v)
        )
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

transitivity_pattern <- function(x) {
  if (is.na(x)) {
    out <- NA_character_
  } else {
    split_x <- substring(x, seq(nchar(x)), seq(nchar(x)))
    out <- seq(0, length(split_x)) %>%
      map(~ append(split_x, as.character(length(split_x) + 1), .x)) %>%
      map(~ paste(.x, collapse = "")) %>%
      unlist()
  }
  return(out)
}
