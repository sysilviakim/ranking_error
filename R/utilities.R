# Library and function calls ===================================================
library(plyr)
library(MASS)
library(here)
library(combinat) # For permn
library(gtools) # For permute
library(tidyselect)
library(tidyverse)
library(styler)
library(PLMIX) # For switch_ord_rank()
library(patchwork)
library(janitor) # For clean_names
library(scales) # For percent
library(assertthat)
library(pwr)
library(xtable)
library(estimatr)
library(ggpubr)
library(Kmisc) ## None-CRAN package. Will wean eventually
library(lubridate)
library(haven)

# Read all functions
source(here("R", "rpluce.R"))
source(here("R", "yougov_import.R"))
source(here("R", "vis_ranking.R"))
source(here("R", "imprr.R"))

# Parameters/stored vectors ====================================================
bootstrap_n <- 1000
root_var <- c(
  tate = "123",
  identity = "1234",
  polar = "12345",
  esystems = "123456"
)
## Previously...
# x <- c(
#   `1` = "Gender", `2` = "City", `3` = "Country", `4` = "Socioeconomic Status",
#   `5` = "Racial or Ethnic Group", `6` = "Political Party", `7` = "Religion"
# )
# match(x, sort(x)) %>% paste(collapse = "") ## 3127546

option_crosswalk <- c(
  ## Tate
  policy = "app_tate_1",
  pork = "app_tate_2",
  service = "app_tate_3",
  ## Electoral systems
  account_pol = "app_esystems_1",
  account_gov = "app_esystems_2",
  vote_seat = "app_esystems_3",
  women = "app_esystems_4",
  minority = "app_esystems_5",
  policy_match = "app_esystems_6",
  ## Identity
  party = "app_identity_1",
  religion = "app_identity_2",
  gender = "app_identity_3",
  race = "app_identity_4",
  ## Polarization
  politicians = "app_polar_1",
  print_media = "app_polar_2",
  social_media = "app_polar_3",
  interest_groups = "app_polar_4",
  citizens = "app_polar_5"
)

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
recover_recorded_rankings <- function(presented_order, true_order, df = NULL) {
  if (is.null(df)) {
    ## Expect as inputs simple strings such as "312" "321"
    presented_order <- strsplit(presented_order, "")[[1]]
    true_order <- strsplit(true_order, "")[[1]]
    recovered_order <- vector("character", length = length(true_order))

    # Iterate through each character in the respondent's response string
    for (i in seq(length(presented_order))) {
      recovered_order[i] <- true_order[[as.numeric(presented_order[i])]]
    }

    # Concatenate the characters
    # to form a string representing the recovered order
    return(paste(recovered_order, collapse = ""))
  } else {
    if (!(presented_order %in% names(df))) {
      stop("Presented order variable is not in the dataframe.")
    }
    if (!(true_order %in% names(df))) {
      stop("Response order variable is not in the dataframe.")
    }

    variable_name <- gsub("_row_rnd", "_recorded", presented_order)
    presented_order <- df[[presented_order]] %>%
      map(~ strsplit(.x, "")[[1]])
    true_order <- df[[true_order]] %>%
      map(~ strsplit(.x, "")[[1]])

    recovered_order <- seq(length(true_order)) %>%
      map(
        ~ {
          out <- vector("character", length = length(true_order[[.x]]))
          if (any(is.na(true_order[[.x]]))) {
            out <- NA
            return(out)
          } else {
            for (i in seq(length(presented_order[[.x]]))) {
              out[i] <- true_order[[.x]][[as.numeric(presented_order[[.x]][i])]]
            }
            return(paste(out, collapse = ""))
          }
        }
      ) %>%
      unlist()

    df[[variable_name]] <- recovered_order
    return(df)
  }
}

## Plot the distribution of recorded values (over permutation space)
plot_dist_ranking <- function(x, ylim = 0.315) {
  J <- nchar(as.character(x$ranking[[1]]))
  x %>%
    ggplot(aes(x = ranking, y = prop, fill = "1")) +
    geom_col() +
    scale_fill_manual(values = "firebrick4") +
    xlab("Recorded Rankings") +
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
  if (all(P0 == P1)) {
    message("Everything is equally distributed.")
  } else {
    message("The chi-square power test result is:")
    print(
      pwr.chisq.test(w = ES.w1(P0, P1), df = (length(tab) - 1), power = power)
    )
  }
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
  out <- deframe(
    enframe(tab) %>%
      bind_rows(
        ., tibble(name = permn(seq(J)) %>%
          map(~ paste(.x, collapse = "")) %>%
          unlist() %>%
          setdiff(., names(tab)), value = as.table(0))
      )
  )
  out <- out[sort(names(out))]
  return(out)
}

## Collapse into resulting ranking pattern in the permutation space
unite_ranking <- function(x, remove = FALSE) {
  x_raw <- x

  ## Find all patterns and bring it to its "root," i.e., without the _1
  var_list <- x %>%
    select(ends_with("_1")) %>%
    names() %>%
    str_sub(end = -3)

  ## Perform for each pattern
  for (v in var_list) {
    ## Is it 3-option, 4-option, 7, 8, ...?
    l <- x %>%
      select(contains(v)) %>%
      select(
        -ends_with("timing"), 
        -ends_with("_rnd")
      )

    if (!grepl("repeat", v)) {
      l <- l %>%
        select(-contains("repeat")) %>%
        ncol()
    } else {
      l <- l %>%
        select(contains("repeat")) %>%
        ncol()
    }

    x <- x %>%
      unite(
        !!as.name(v),
        sep = "", !!as.name(paste0(v, "_1")):!!as.name(paste0(v, "_", l)),
        remove = remove
      ) %>%
      mutate(
        !!as.name(v) := case_when(
          grepl("NANA", !!as.name(v)) ~ NA_character_,
          TRUE ~ !!as.name(v)
        )
      )
  }

  return(x)
}

## Avg. rank compute
avg_rank <- function(df, var, split_only = FALSE) {
  ## What is the J?
  J <- nchar(df[[var]][[1]])
  v <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th")
  out <- df %>%
    select(!!as.name(var)) %>%
    separate(!!as.name(var), sep = seq(9)[1:J], into = v[1:J])
  if (split_only == FALSE) {
    out <- out %>%
      summarise(
        across(
          everything(),
          ~ tibble(
            mean = mean(as.numeric(.x)),
            # sd = sd(as.numeric(.x)),
            se = sd(as.numeric(.x)) / sqrt(nrow(out))
          )
        )
      )
  }
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
  if (!(v1 %in% names(df)) | !(v2 %in% names(df))) {
    stop("One or more of the variables does not exist in the dataframe.")
  }
  ## Something like with(df, table(tate = v1, identity = v2)) fails
  ## Not much point in printing table, I guess...
  print(cor(df[[v1]], df[[v2]], use = "pairwise"))
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

repeat_coherent <- function(x, v1, v2) {
  y1 <- sum(x[[v1]] == x[[v2]], na.rm = TRUE)
  y2 <- sum(x[[v1]] != x[[v2]], na.rm = TRUE)
  return(
    paste0(
      "Repeated task for ", v1, " yields a ",
      round(y1 / (y1 + y2) * 100, digits = 1), "% rate of same answers."
    )
  )
}

venn_diagram_fill <- function(x, v1, v2, v3) {
  ## This is a slapdash function to fill in the Lucidchart Venn diagram
  ## manually; use at discretion
  list(
    x1 = c(1, 0, 0),
    x2 = c(1, 1, 0),
    x3 = c(1, 1, 1),
    x4 = c(1, 0, 1),
    x5 = c(0, 1, 0),
    x6 = c(0, 1, 1),
    x7 = c(0, 0, 1),
    x8 = c(0, 0, 0)
  ) %>%
    map_chr(
      ~ main %>%
        filter(
          !!as.name(v1) == .x[1] &
            !!as.name(v2) == .x[2] &
            !!as.name(v3) == .x[3]
        ) %>%
        nrow() %>%
        {
          formatC(
            round(
              . / nrow(
                main %>%
                  filter(
                    !is.na(!!as.name(v1)) &
                      !is.na(!!as.name(v2)) &
                      !is.na(!!as.name(v3))
                  )
              ) * 100,
              digits = 1
            ),
            digits = 1,
            format = "f"
          )
        }
    )
}

## Not perfectly do-not-repeat-yourself but will return later
pattern_compare_pass_fail <- function(main, v, y_upper = .75, label = NULL,
                                      recorded = TRUE) {
  if (recorded) {
    sfx <- "_recorded"
  } else {
    sfx <- ""
  }
  if (is.null(label)) {
    label <- c("Passed", "Failed")
  }
  out <- root_var %>%
    imap(
      function(x, y) {
        list(
          pass = list(v = 0, lab = label[1]),
          fail = list(v = 1, lab = label[2])
        ) %>%
          map(
            ~ main %>%
              filter(!!as.name(v) == .x$v) %>%
              .[[paste0("app_", y, sfx)]] %>%
              table() %>%
              table_to_tibble() %>%
              plot_dist_ranking(., ylim = y_upper) +
              ggtitle(.x$lab)
          ) %>%
          map(~ plot_nolegend(pdf_default(.x)))
      }
    )

  ## Supplement by no-context questions
  p3 <- list(
    pass = list(v = 0, lab = "Passed"),
    fail = list(v = 1, lab = "Failed")
  ) %>%
    map(
      ~ main %>%
        filter(!is.na(no_context_3_recorded)) %>%
        filter(!!as.name(v) == .x$v) %>%
        .$no_context_3_recorded %>%
        table() %>%
        table_to_tibble() %>%
        plot_dist_ranking(., ylim = y_upper) +
        ggtitle(.x$lab)
    ) %>%
    map(~ plot_nolegend(pdf_default(.x)))

  p4 <- list(
    pass = list(v = 0, lab = "Passed"),
    fail = list(v = 1, lab = "Failed")
  ) %>%
    map(
      ~ main %>%
        filter(!is.na(no_context_4_recorded)) %>%
        filter(!!as.name(v) == .x$v) %>%
        .$no_context_4_recorded %>%
        table() %>%
        table_to_tibble() %>%
        plot_dist_ranking(., ylim = y_upper) +
        ggtitle(.x$lab)
    ) %>%
    map(~ plot_nolegend(pdf_default(.x)))

  out$no_context_3 <- p3
  out$no_context_4 <- p4

  return(out)
}

viz_avg <- function(data) {
  J <- nrow(data) / 2
  p <- data %>%
    rowwise() %>%
    mutate(imp = simple_cap(imp)) %>%
    mutate(name = simple_cap(name)) %>%
    mutate(
      name = case_when(
        name == "Print_media" ~ "Print Media and TV",
        name == "Social_media" ~ "Social Media",
        name == "Interest_groups" ~ "Interest Groups",
        name == "Account_pol" ~ "Accountability (Politicians)",
        name == "Account_gov" ~ "Accountability (Govt)",
        name == "Policy_match" ~ "Median Voter Policy",
        name == "Vote_seat" ~ "Vote-seat Consistency",
        name == "Women" ~ "Women's Representation",
        name == "Minority" ~ "Minority Representation",
        TRUE ~ name
      ),
      name = str_pad(name, width = 28)
    ) %>%
    ungroup() %>%
    rename(Type = imp) %>%
    mutate(name = fct_reorder(name, est)) %>%
    ggplot(., aes(x = fct_rev(name), y = est, color = Type)) +
    geom_point(
      aes(shape = Type),
      size = 2,
      position = position_dodge(width = 0.5)
    ) +
    # Reorder by point estimate
    geom_linerange(
      aes(ymin = low, ymax = up),
      lwd = 1, position = position_dodge(width = 0.5)
    ) +
    scale_color_manual(values = c("#b0015a", "#999999")) +
    theme_bw() +
    coord_flip() +
    xlab("") +
    ylab("") +
    ylim(1, J) +
    geom_hline(yintercept = (J + 1) / 2, linetype = "dashed")
  return(
    pdf_default(p) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"),
        legend.spacing.x = unit(0, "cm"),
        legend.spacing.y = unit(0, "cm"),
        plot.title = element_text(face = "bold")
      )
  )
}
