# Library and function calls ===================================================
library(plyr)
library(MASS)
library(here)
library(combinat) # For permn
library(gtools) # For permute
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

# READING THE FUNCTION THAT DRAW FROM PLACKETT-LUCE
source(here("R", "rpluce.R"))

# Parameters/stored vectors ====================================================
qualtrics_meta <- c(
  "start_date", "end_date", "status", "ip_address", "progress",
  "duration_in_seconds", "finished", "recorded_date", "response_id",
  "recipient_last_name", "recipient_first_name", "recipient_email",
  "external_reference", "location_latitude", "location_longitude",
  "distribution_channel", "user_language", "q_recaptcha_score", "consent"
)
bootstrap_n <- 1000
root_var <- c(
  tate = "123",
  e_systems = "1234567",
  identity = "1234567",
  polar = "12345678"
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
  account_pol = "app_e_systems_1",
  account_gov = "app_e_systems_2",
  stable = "app_e_systems_3",
  prop = "app_e_systems_4",
  women = "app_e_systems_5",
  minority = "app_e_systems_6",
  median = "app_e_systems_7",
  ## Identity
  party = "app_identity_1",
  job = "app_identity_2",
  religion = "app_identity_3",
  gender = "app_identity_4",
  family_role = "app_identity_5",
  race = "app_identity_6",
  American = "app_identity_7",
  ## Polarization
  dem_pol = "app_polar_1",
  rep_pol = "app_polar_2",
  media = "app_polar_3",
  social_media = "app_polar_4",
  interest_groups = "app_polar_5",
  dem_citizens = "app_polar_6",
  rep_citizens = "app_polar_7",
  e_systems = "app_polar_8"
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

## import and wrangle Qualtrics data
qualtrics_import <- function(fname) {
  ## Exported using not the choice text but numeric values
  df_raw <- read_csv(here("data", "raw", fname), show_col_types = FALSE) %>%
    clean_names() %>%
    filter(
      start_date != "Start Date" &
        start_date != '{"ImportId":"startDate","timeZone":"America/Denver"}'
    ) %>%
    mutate(
      duration_in_seconds = as.numeric(duration_in_seconds),
      duration_in_minutes = duration_in_seconds / 60,
      q_recaptcha_score = as.numeric(q_recaptcha_score)
    )

  ## Status needs to be "IP Address" which is 0.
  temp <- df_raw %>%
    filter(status == "0") %>%
    ## Make consistent with root_var
    rename_with(~ gsub("polarization", "polar", .x)) %>%
    rename_with(~ gsub("affective_polar", "polar", .x)) %>%
    rename_with(~ gsub("aff_polar", "polar", .x)) %>%
    rename_with(~ gsub("_1993", "", .x)) %>%
    rename_with(~ gsub("ternovsky", "ternovski", .x))

  ## If IP addresses overlap, that's a major red flag so stop
  ## assert_that(!any(duplicated(main$ip_address)))
  if (any(duplicated(temp$ip_address))) {
    message("There are duplicates in IP addresses. Please investigate.")
  }

  message(paste0("We have total ", nrow(temp), " respondents."))

  ## Separate out timing variables to actual responses
  timing <- temp %>%
    select(
      response_id,
      matches(qualtrics_meta %>% paste(collapse = "|")),
      contains("timing"), contains("time")
    ) %>%
    rename_with(~ gsub("timing_", "time_", .x))

  main <- temp %>%
    select(-contains("timing"), -contains("time")) %>%
    select(
      -matches(
        setdiff(qualtrics_meta, c("q_recaptcha_score", "response_id")) %>%
          paste(collapse = "|")
      )
    )

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
    ## Remember, this will bring out the true ranking
    ## and not the *observed* ranking of the respondent
    ## (i.e., permutation pattern)
    ## unless the randomization has given a 1234... order

    ## So if the true order respondent gave out  is 3-2-1
    ## but the order provided is                    3-1-2
    ## the *observed* ranking is                    1-3-2
    ## unite_ranking() will give 321 for this respondent! The true ranking!

    ## Note that Qualtrics' "View Response" will always align the
    ## randomized orders into the original order and show the true ranking
    ## in the "recovered" fashion

    ## If the true order respondent gave out is     6-3-2-7-1-4-5
    ## but the order provided is                    6-1-5-2-7-3-4
    ## the true order respondent provided is        4-6-1-3-5-2-7

    ## IMPORTANT: THIS REQUIRES THAT THE RECODING IS IN NATURAL PROGRESSION
    ## i.e., the reference set is 1-2-3-4-5-6-7
    ## If it is 1-2-3-4-6-5-7 or something that's different, doesn't work

    unite_ranking() %>%
    ## Recovered observed rankings
    recover_observed_ranking(
      "no_context_3_options_do", "no_context_3_options", .
    ) %>%
    recover_observed_ranking(
      "no_context_4_options_do", "no_context_4_options", .
    ) %>%
    recover_observed_ranking("app_tate_do", "app_tate", .) %>%
    recover_observed_ranking("app_e_systems_do", "app_e_systems", .) %>%
    recover_observed_ranking("app_identity_do", "app_identity", .) %>%
    recover_observed_ranking("app_polar_do", "app_polar", .) %>%
    ## For anchor questions, too
    recover_observed_ranking("anc_tate_do", "anc_tate", .) %>%
    recover_observed_ranking("anc_e_systems_do", "anc_e_systems", .) %>%
    recover_observed_ranking("anc_identity_do", "anc_identity", .) %>%
    recover_observed_ranking("anc_polar_do", "anc_polar", .)

  ## Create binary indicators for partial rankers + attention check fails
  main <- main %>%
    ## Geometric patterns + attention check fails + repeat task fails
    mutate(
      ternovski_fail = case_when(ternovski_screener2 != "1,2" ~ 1, TRUE ~ 0),
      berinsky_fail = case_when(berinsky_screener != "4,12" ~ 1, TRUE ~ 0),
      ## Geometric patterns: applications (ns is for old term "non-sincere")
      ns_tate = case_when(anc_tate != "123" ~ 1, TRUE ~ 0),
      ns_e_systems = case_when(anc_e_systems != "1234567" ~ 1, TRUE ~ 0),
      ns_identity = case_when(anc_identity != "1234567" ~ 1, TRUE ~ 0),
      ns_polar = case_when(anc_polar != "12345678" ~ 1, TRUE ~ 0),
      ## Perfectly collinear variables, just renamed/recoded for easy access
      anc_correct_tate = case_when(anc_tate == "123" ~ 1, TRUE ~ 0),
      anc_correct_e_systems = case_when(anc_e_systems == "1234567" ~ 1, TRUE ~ 0),
      anc_correct_identity = case_when(anc_identity == "1234567" ~ 1, TRUE ~ 0),
      anc_correct_polar = case_when(anc_polar == "12345678" ~ 1, TRUE ~ 0),
      ## Repeat coherence: applications
      ## 1 means incoherent!
      repeat_tate = case_when(
        app_tate == app_tate_repeat ~ 0,
        app_tate != app_tate_repeat ~ 1
      ),
      repeat_e_systems = case_when(
        app_e_systems == app_e_systems_repeat ~ 0,
        app_e_systems != app_e_systems_repeat ~ 1
      ),
      repeat_identity = case_when(
        app_identity == app_identity_repeat ~ 0,
        app_identity != app_identity_repeat ~ 1
      ),
      repeat_polar = case_when(
        app_polar == app_pol_repeat ~ 0,
        app_polar != app_pol_repeat ~ 1
      )
    ) %>%
    rowwise() %>%
    mutate(
      app_tate_trunc1 = str_sub(app_tate, 1, 1),
      app_tate_trunc1_repeat = str_sub(app_tate_repeat, 1, 1),
      app_e_systems_trunc1 = str_sub(app_e_systems, 1, 1),
      app_e_systems_trunc1_repeat = str_sub(app_e_systems_repeat, 1, 1),
      app_identity_trunc1 = str_sub(app_identity, 1, 1),
      app_identity_trunc1_repeat = str_sub(app_identity_repeat, 1, 1),
      app_polar_trunc1 = str_sub(app_polar, 1, 1),
      app_polar_trunc1_repeat = str_sub(app_pol_repeat, 1, 1),
      app_e_systems_trunc2 = str_sub(app_e_systems, 1, 2),
      app_e_systems_trunc2_repeat = str_sub(app_e_systems_repeat, 1, 2),
      app_identity_trunc2 = str_sub(app_identity, 1, 2),
      app_identity_trunc2_repeat = str_sub(app_identity_repeat, 1, 2),
      app_polar_trunc2 = str_sub(app_polar, 1, 2),
      app_polar_trunc2_repeat = str_sub(app_pol_repeat, 1, 2),
      app_tate_trunc3 = str_sub(app_tate, 1, 3),
      app_tate_trunc3_repeat = str_sub(app_tate_repeat, 1, 3),
      app_e_systems_trunc3 = str_sub(app_e_systems, 1, 3),
      app_e_systems_trunc3_repeat = str_sub(app_e_systems_repeat, 1, 3),
      app_identity_trunc3 = str_sub(app_identity, 1, 3),
      app_identity_trunc3_repeat = str_sub(app_identity_repeat, 1, 3),
      app_polar_trunc3 = str_sub(app_polar, 1, 3),
      app_polar_trunc3_repeat = str_sub(app_pol_repeat, 1, 3),

      # repeat_trunc_e_systems = case_when(
      #   app_e_systems_trunc == app_e_systems_trunc_repeat ~ 0,
      #   app_e_systems_trunc != app_e_systems_trunc_repeat ~ 1
      # ),
      # repeat_trunc_identity = case_when(
      #   app_identity_trunc == app_identity_trunc_repeat ~ 0,
      #   app_identity_trunc != app_identity_trunc_repeat ~ 1
      # ),
      # repeat_trunc_polar = case_when(
      #   app_polar_trunc == app_polar_trunc_repeat ~ 0,
      #   app_polar_trunc != app_polar_trunc_repeat ~ 1
      # )
    ) %>%
    ungroup() %>%
    ## Partial rankers
    mutate(
      partial_tate_main =
        case_when(grepl("9", app_tate) ~ 1, TRUE ~ 0),
      partial_tate_anc =
        case_when(grepl("9", anc_tate) ~ 1, TRUE ~ 0),
      partial_e_systems_main =
        case_when(grepl("9", app_e_systems) ~ 1, TRUE ~ 0),
      partial_e_systems_anc =
        case_when(grepl("9", anc_e_systems) ~ 1, TRUE ~ 0),
      partial_identity_main =
        case_when(grepl("9", app_identity) ~ 1, TRUE ~ 0),
      partial_identity_anc =
        case_when(grepl("9", anc_identity) ~ 1, TRUE ~ 0),
      partial_polar_main =
        case_when(grepl("9", app_polar) ~ 1, TRUE ~ 0),
      partial_polar_anc =
        case_when(grepl("9", anc_polar) ~ 1, TRUE ~ 0)
    ) %>%
    ## pid7
    mutate(
      pid7 = case_when(
        pid3 == "1" & pid7_dem == "1" ~ "Strong Democrat",
        pid3 == "1" & pid7_dem == "2" ~ "Not so strong Democrat",
        pid3 == "3" & pid7_ind == "2" ~ "Leaning Democrat",
        pid3 == "3" & pid7_ind == "3" ~ "Independent",
        pid3 == "3" & pid7_ind == "1" ~ "Leaning Republican",
        pid3 == "2" & pid7_rep == "2" ~ "Not so strong Republican",
        pid3 == "2" & pid7_rep == "1" ~ "Strong Republican"
      ),
      ## Including leaners
      pid3alt = case_when(
        grepl("Democrat", pid7) ~ "Dem",
        grepl("Republican", pid7) ~ "Rep",
        TRUE ~ "Ind"
      ),
      white = case_when(
        race == "1" ~ "White",
        TRUE ~ "Non-white"
      )
    )

  return(list(main = main, timing = timing, raw = df_raw))
}

## Recover the reference (true) ranking
## with respect to the reference item set (here: {abc})
recover_observed_ranking <- function(presented_order, true_order, df = NULL) {
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

    variable_name <- gsub("_do", "_observed", presented_order)
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
    ## Is it 3-option, 4-option, 7, 8, ...?
    l <- x %>%
      select(contains(v)) %>%
      select(-ends_with("_do"))

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
      mutate(
        across(
          !!as.name(paste0(v, "_1")):!!as.name(paste0(v, "_", l)),
          ~ case_when(.x == "-99" ~ "9", TRUE ~ .x)
        )
      ) %>%
      unite(
        !!as.name(v),
        sep = "", !!as.name(paste0(v, "_1")):!!as.name(paste0(v, "_", l)),
        remove = FALSE
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
      ## names(x_raw %>% select(start_date:berinsky_screener)),
      ## Lucid generated
      rid, age_2, gender_2, hhi, ethnicity, hispanic, education_2,
      political_party, region, zip,
      everything()
    )

  return(x)
}

## Avg. rank compute
avg_rank <- function(df, var, split_only = FALSE) {
  l <- nchar(df[[var]][[1]])
  v <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th")
  out <- df %>%
    select(!!as.name(var)) %>%
    separate(!!as.name(var), sep = seq(9)[1:l], into = v[1:l])
  if (split_only == FALSE) {
    out <- out %>%
      summarise(across(everything(), ~ mean(as.numeric(.x))))
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


vis_r <- function(data,
                  target_item,
                  other_items,
                  treat = NULL,
                  single_plot = TRUE) {
  #' @description  \code{est_r} visualizes all ATEs in a typical effect set
  #'
  #' @param data A dataset
  #' @param target_item A string for the target item
  #' @param other_items A set of strings for other items of interest
  #'
  #' @return A ggplot that visualizes all treatment effects
  #' @examples
  #' dt <- read_csv("ex_police.csv")
  #' my_target_item <- "victim"
  #' my_other_items <- c("officers", "PDchief", "mayor", "DA", "governor", "senators")
  #' est_r(data = dt,
  #'       treat = my_treat,
  #'       target_item = my_target_item,
  #'       other_items = my_other_items)
  #' @export

  use_col <- c("#b0015a", "black")
  label <- simple_cap(gsub("_", " ", simple_cap(target_item)))

  # Size
  N <- dim(data)[1]

  # Treatment indicator
  if (!is.null(treat)) {
    D <- data[treat] %>% pull()
  }

  # Process raw ranking data
  J_1 <- length(other_items) # J - 1
  J <- J_1 + 1

  dt <- data

  Y_rank_target <- dt[target_item] %>% pull() # Average ranks
  Y_rank_others <- list()
  for (i in 1:J_1) {
    Y_rank_others[[i]] <- dt[other_items[i]] %>% pull()
  }

  Y_pairwise <- list() # Pairwise ranking P
  for (i in 1:J_1) {
    compar <- dt[other_items[i]] %>% pull() # Comparison item
    Y_pairwise[[i]] <- ifelse(Y_rank_target < compar, 1, 0)
  }

  Y_top1 <- ifelse(Y_rank_target <= 1, 1, 0) # Top-1 ranking Probability (P)
  Y_top2 <- ifelse(Y_rank_target <= 2, 1, 0) # Top-2 ranking P
  Y_top3 <- ifelse(Y_rank_target <= 3, 1, 0) # Top-3 ranking P
  Y_top4 <- ifelse(Y_rank_target <= 4, 1, 0) # Top-4 ranking P
  Y_top5 <- ifelse(Y_rank_target <= 5, 1, 0) # Top-5 ranking P
  Y_top6 <- ifelse(Y_rank_target <= 6, 1, 0) # Top-6 ranking P
  Y_top7 <- ifelse(Y_rank_target <= 7, 1, 0) # Top-7 ranking P

  Y_marginal <- list()
  tgt <- dt[target_item] %>% pull()
  for (i in 1:J) {
    Y_marginal[[i]] <- ifelse(tgt == i, 1, 0)
  }

  # Collect estimated means: without treatment
  if (is.null(treat)) {
    # Estimate baseline outcome values via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ 1) %>% tidy()
    m_rank_others <- list()
    for (i in 1:J_1) {
      m_rank_others[[i]] <- lm_robust(Y_rank_others[[i]] ~ 1) %>% tidy()
    }

    m_top1 <- lm_robust(Y_top1 ~ 1) %>% tidy()
    m_top2 <- lm_robust(Y_top2 ~ 1) %>% tidy()
    m_top3 <- lm_robust(Y_top3 ~ 1) %>% tidy()
    m_top4 <- lm_robust(Y_top4 ~ 1) %>% tidy()
    m_top5 <- lm_robust(Y_top5 ~ 1) %>% tidy()
    m_top6 <- lm_robust(Y_top6 ~ 1) %>% tidy()
    m_top7 <- lm_robust(Y_top7 ~ 1) %>% tidy()

    m_pairwise <- list()
    for (i in 1:J_1) {
      m_pairwise[[i]] <- lm_robust(Y_pairwise[[i]] ~ 1) %>% tidy()
    }

    m_marginal <- list()
    for (i in 1:J) {
      m_marginal[[i]] <- lm_robust(Y_marginal[[i]] ~ 1) %>% tidy()
    }
    m_rank_catch <- do.call(rbind.data.frame, m_rank_others) %>%
      mutate(
        outcome = paste0(other_items),
        target = "B"
      )
    m_rank <- m_rank_target %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )

    m_rank_catch <- do.call(rbind.data.frame, m_rank_others) %>%
      mutate(
        outcome = paste0(other_items),
        target = "B"
      )
    m_rank <- m_rank_target %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )
    gg_averagerank <- rbind(m_rank, m_rank_catch)

    gg_pairwise <- do.call(rbind.data.frame, m_pairwise) %>%
      mutate(outcome = paste0("v.", " ", other_items))

    gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
      mutate(outcome = paste0("Ranked", " ", 1:J))

    gg_topk <- rbind(
      m_top1, m_top2, m_top3, m_top4,
      m_top5, m_top6, m_top7
    ) %>%
      mutate(
        outcome = c(
          "Top-1", "Top-2", "Top-3", "Top-4",
          "Top-5", "Top-6", "Top-7"
        )
      ) %>%
      select(outcome, everything()) %>%
      ## Limit to less than 7 if J<7
      .[1:(J - 1), ]

    # Visualize all effects
    p_avg <- ggplot(
      gg_averagerank,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = target), size = 2) +
      geom_linerange(
        aes(
          x = outcome, ymin = conf.low, ymax = conf.high,
          color = target
        ),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      ylab("") +
      xlab("") +
      ylim(1, (1 + J_1)) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("A. Average Ranks")) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    p_pair <- ggplot(
      gg_pairwise,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(aes(x = outcome, ymin = conf.low, ymax = conf.high),
        lwd = 1
      ) +
      ylab("") +
      xlab("") +
      ylim(0, 1) +
      coord_flip() +
      theme_bw() +
      ggtitle(
        paste0("B. Pairwise Ranking of", " ", label, " ", "Over Other Options")
      ) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    p_topk <- ggplot(
      gg_topk,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(aes(x = outcome, ymin = conf.low, ymax = conf.high),
        lwd = 1
      ) +
      ylab("") +
      xlab("") +
      ylim(0, 1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("C. Top-k Ranking of", " ", label)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    p_marginal <- ggplot(
      gg_marginal,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(aes(x = outcome, ymin = conf.low, ymax = conf.high),
        lwd = 1
      ) +
      ylab("") +
      xlab("") +
      ylim(0, 1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("D. Marginal Ranking of", " ", label)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    if (single_plot == TRUE) {
      ggpubr::ggarrange(p_avg, p_pair, p_topk, p_marginal)
    } else {
      return(
        list(
          p_avg = p_avg,
          p_pair = p_pair,
          p_topk = p_topk,
          p_marginal = p_marginal
        )
      )
    }
  } else {
    # Prep for visualization
    scenario <- list(
      c("Not_significant", "Negative", "Positive"),
      c("Not_significant", "Negative"),
      c("Not_significant", "Positive"),
      c("Negative", "Positive"),
      c("Not_significant"),
      c("Negative"),
      c("Positive")
    )
    scena_col <- list(
      c("gray", "#b0015a", "#128ba0"),
      c("gray", "#b0015a"),
      c("gray", "#128ba0"),
      c("#b0015a", "#128ba0"),
      c("gray"),
      c("#b0015a"),
      c("#128ba0")
    )

    av_scenario <- list(
      c("Not_significant", "Negative", "Positive"),
      c("Not_significant", "Negative"),
      c("Not_significant", "Positive"),
      c("Negative", "Positive"),
      c("Not_significant"),
      c("Negative"),
      c("Positive")
    )
    av_scena_col <- list(
      c("gray", "#128ba0", "#b0015a"),
      c("gray", "#128ba0"),
      c("gray", "#b0015a"),
      c("#128ba0", "#b0015a"),
      c("gray"),
      c("#128ba0"),
      c("#b0015a")
    )

    # Estimate ATEs with Difference-in-means via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ D) %>% tidy()

    m_top1 <- lm_robust(Y_top1 ~ D) %>% tidy()
    m_top2 <- lm_robust(Y_top2 ~ D) %>% tidy()
    m_top3 <- lm_robust(Y_top3 ~ D) %>% tidy()
    m_top4 <- lm_robust(Y_top4 ~ D) %>% tidy()
    m_top5 <- lm_robust(Y_top5 ~ D) %>% tidy()
    m_top6 <- lm_robust(Y_top6 ~ D) %>% tidy()
    m_top7 <- lm_robust(Y_top7 ~ D) %>% tidy()

    m_pairwise <- list()
    for (i in 1:J_1) {
      m_pairwise[[i]] <- lm_robust(Y_pairwise[[i]] ~ D) %>% tidy()
    }

    m_marginal <- list()
    for (i in 1:J) {
      m_marginal[[i]] <- lm_robust(Y_marginal[[i]] ~ D) %>% tidy()
    }

    m_rank <- m_rank_target %>%
      filter(term == "D") %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )
    gg_averagerank <- rbind(m_rank)

    gg_pairwise <- do.call(rbind.data.frame, m_pairwise) %>%
      filter(term == "D") %>%
      mutate(outcome = paste0("v.", " ", other_items))

    gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
      filter(term == "D") %>%
      mutate(outcome = paste0("Ranked", " ", 1:J))

    gg_topk <- rbind(
      m_top1, m_top2, m_top3, m_top4,
      m_top5, m_top6, m_top7
    ) %>%
      filter(term == "D") %>%
      mutate(outcome = c(
        "Top-1", "Top-2", "Top-3", "Top-4",
        "Top-5", "Top-6", "Top-7"
      ))

    # Visualize all effects
    gg_averagerank$col <- ifelse(
      gg_averagerank$conf.low > 0, "Positive", "Not_significant"
    )
    gg_averagerank$col <- ifelse(
      gg_averagerank$conf.high < 0, "Negative", gg_averagerank$col
    )
    names(av_scena_col) <- av_scenario
    pattern <- unique(gg_averagerank$col) # Observed pattern
    use_col <- av_scena_col[pattern] # Use this color pallet

    p_avg <- ggplot(
      gg_averagerank,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_colour_manual(values = use_col) +
      ylab("") +
      xlab("") +
      #  ylim(1,(1+J_1)) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("A. Average Ranks")) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    gg_pairwise$col <- ifelse(
      gg_pairwise$conf.low > 0, "Positive", "Not_significant"
    )
    gg_pairwise$col <- ifelse(
      gg_pairwise$conf.high < 0, "Negative", gg_pairwise$col
    )
    names(scena_col) <- scenario
    pattern <- unique(gg_pairwise$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_pair <- ggplot(
      gg_pairwise,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("") +
      xlab("") +
      #  ylim(0,1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("B. Pairwise Ranking of", " ", target_item, " ", "Over")) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    gg_topk$col <- ifelse(gg_topk$conf.low > 0, "Positive", "Not_significant")
    gg_topk$col <- ifelse(gg_topk$conf.high < 0, "Negative", gg_topk$col)
    names(scena_col) <- scenario
    pattern <- unique(gg_topk$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_topk <- ggplot(
      gg_topk,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("") +
      xlab("") +
      #  ylim(0,1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("C. Top-k Ranking of", " ", target_item)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    gg_marginal$col <- ifelse(
      gg_marginal$conf.low > 0, "Positive", "Not_significant"
    )
    gg_marginal$col <- ifelse(
      gg_marginal$conf.high < 0, "Negative", gg_marginal$col
    )
    names(scena_col) <- scenario
    pattern <- unique(gg_marginal$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_marginal <- ggplot(
      gg_marginal,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("") +
      xlab("") +
      #  ylim(0,1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("D. Marginal Ranking of", " ", target_item)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    if (single_plot == TRUE) {
      ggpubr::ggarrange(p_avg, p_pair, p_topk, p_marginal)
    } else {
      return(
        list(
          p_avg = p_avg,
          p_pair = p_pair,
          p_topk = p_topk,
          p_marginal = p_marginal
        )
      )
    }
  }
}

imprr <- function(data, # all data
                  rank_q, # string for ranking names
                  main_q,
                  anchor, # string for anchor names
                  anc_correct, # string for correctness indicator
                  J # number of items
) {
  # # Do not run

  # data <- dt_id
  # rank_q <- c("party", "job", "religion", "gender",
  #             "family_role", "race", "American")
  # main_q <- "app_identity"
  # anchor <- "anc_identity"
  # anc_correct <- "anc_correct"
  # J <- 7

  # data = dt_rep
  # rank_q = c("policy", "pork", "service")
  # main_q = "app_tate"
  # anchor = "anc_tate"
  # anc_correct = "anc_correct"
  # J = 3

  N <- dim(data)[1]

  # Equation (8) -- proportion of random answers
  Corr <- data[anc_correct] %>% pull()
  adjust <- mean(Corr) - 1 / factorial(J)
  normalizer <- (1 - 1 / factorial(J))
  p_non_random <- adjust / normalizer

  # Equation (9) -- distribution of random answers

  # Generate PMF of anchor rankings
  temp <- table(data[anchor])
  length(temp) # 60 unique ranking out of 5040
  perm_j <- combinat::permn(1:J)
  perm_j <- do.call(rbind.data.frame, perm_j)
  colnames(perm_j) <- c(paste0("position_", 1:J))
  perm_j <- perm_j %>% unite(col = "match", sep = "")

  obs_anchor_freq <- table(data[anchor]) %>%
    data.frame()
  colnames(obs_anchor_freq) <- c("match", "Freq")

  f_anchor <- perm_j %>%
    left_join(obs_anchor_freq) %>%
    mutate(Freq = ifelse(is.na(Freq), 0, Freq)) # Impute 0s

  f_anc_true <- data.frame(perm_j, 0)
  f_anc_true[1, "X0"] <- 1 # True density for anchor -- 1234567

  A <- f_anchor$Freq / N # -- empirical pmf of rankings in anchor
  B <- p_non_random # -- estimated proportion of non-random
  C <- f_anc_true$X0 # -- true pmf of rankings in anchor

  f_random <- (A - (B * C)) / (1 - B)

  sum(f_random) # This must be 1

  # # Alternatively, asymptotics gives us
  # f_random <- rep(1/factorial(J), factorial(J))
  # sum(f_random) # This must be 1


  # Equation (10) -- distribution of error-free rankings
  obs_main_freq <- table(data[main_q]) %>%
    data.frame()
  colnames(obs_main_freq) <- c("match", "Freq")

  f_main <- perm_j %>%
    left_join(obs_main_freq) %>%
    mutate(Freq = ifelse(is.na(Freq), 0, Freq)) # Impute 0.0001

  D <- f_main$Freq / N # -- empirical pmf of ranking in the main question
  E <- f_random # -- estimated pmf of errors in the anchor

  f_true <- (D - ((1 - B) * E)) / B

  sum(f_true) # This must be 1

  # Equation 4
  w <- f_true / D # weight vector
  # w[which(!is.finite(w))] <- 0

  w_frame <- data.frame(
    main = perm_j$match,
    weight = w
  )
  colnames(w_frame) <- c(main_q, "weight")

  data_w <- data %>%
    left_join(w_frame) %>%
    mutate(p_non_random = p_non_random)
}

## Not perfectly do-not-repeat-yourself but will return later
pattern_compare_pass_fail <- function(main, v, y_upper = .75) {
  out <- root_var %>%
    imap(
      function(x, y) {
        list(
          pass = list(v = 0, lab = "Passed"), 
          fail = list(v = 1, lab = "Failed")
        ) %>% 
          map(
            ~ main %>%
              filter(!!as.name(v) == .x$v) %>%
              .[[paste0("app_", y, "_observed")]] %>%
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
        filter(!is.na(no_context_3_options_observed)) %>%
        filter(!!as.name(v) == .x$v) %>%
        .$no_context_3_options_observed %>%
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
        filter(!is.na(no_context_4_options_observed)) %>%
        filter(!!as.name(v) == .x$v) %>%
        .$no_context_4_options_observed %>%
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

