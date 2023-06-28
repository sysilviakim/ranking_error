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

# Read all functions
source(here("R", "rpluce.R"))
source(here("R", "vis_ranking.R"))
source(here("R", "weight_patterns.R"))

# Parameters/stored vectors ====================================================
bootstrap_n <- 1000
root_var <- c(
  tate = "123",
  esystems = "1234567",
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
yougov_import <- function(fname) {
  if (grepl(".csv", fname)) {
    df_raw <- read_csv(here("data", "raw", fname), show_col_types = FALSE)
  } else {
    df_raw <- read_sav(here("data", "raw", fname), show_col_types = FALSE)
  }
  ## Exported using not the choice text but numeric values
  df_raw <- df_raw %>%
    clean_names() %>%
    mutate(response_id = str_pad(row_number(), width = 4, pad = "0")) %>%
    select(response_id, everything())
  
  ## Metadata
  yougov_meta <- c(
    "version", "exit_status", "respondent_status", "disposition", "last_page",
    "endtime", "favor_or_oppose", "follow_up", "hcountry_code", "perc_skipped",
    "phone_flag", "points", 
    ## I don't know what these are + must request browser types
    "posneg", "r1", "r2", "r3", "r4", "r5", "r6",
    "attention_check", "attention_check_word",
    "capabilities_touch", "invalidrank", "more" 
  )

  # Check for timing variables
  # Check for duration + start time + end time

  # filter(
  #   start_date != "Start Date" &
  #     start_date != '{"ImportId":"startDate","timeZone":"America/Denver"}'
  # ) %>%
  # mutate(
  #   duration_in_seconds = as.numeric(duration_in_seconds),
  #   duration_in_minutes = duration_in_seconds / 60,
  #   q_recaptcha_score = as.numeric(q_recaptcha_score)
  # )

  ## Aggressively rename variables 
  ## Next time, alert YouGov to preferred module/page names!
  temp <- df_raw %>%
    ## Make consistent with root_var
    ## Fix typos and inconsistencies in names
    rename_with(~ gsub("order", "_order", .x)) %>%
    rename_with(~ gsub("__", "_", .x)) %>%
    rename_with(~ gsub("polarization", "polar", .x)) %>%
    rename_with(~ gsub("identityhopkins", "anc_identity", .x)) %>%
    rename_with(~ gsub("holidays", "anc_polar", .x)) %>%
    rename_with(~ gsub("^alpha_", "anc_id_alphabet_", .x)) %>%
    rename_with(~ gsub("^exact_", "anc_id_exact_", .x)) %>%
    rename_with(~ gsub("^esystems_", "anc_esystems_", .x)) %>%
    rename_with(~ gsub("affectivepolar", "_polar", .x)) %>%
    rename_with(~ gsub("_1993", "", .x)) %>%
    rename_with(~ gsub("TernovskyScreener", "ternovski", .x)) %>%
    rename_with(~ gsub("apsa_", "esystems_", .x)) %>%
    rename_with(~ gsub("ternovsky_screener2", "ternovski", .x)) %>%
    rename_with(~ gsub("berinskyscreener", "berinsky", .x)) %>%
    rename_with(~ gsub("estemporal", "anc_es_temporal", .x)) %>%
    rename_with(~ gsub("esalphabet", "anc_es_alphabet", .x)) %>%
    rename_with(~ gsub("esystems_app", "app_esystems", .x)) %>%
    rename_with(~ gsub("anc_app_esystems", "app_esystems", .x)) %>%
    ## ordering variables
    rename_with(~ gsub("hopkins_order", "identity_order", .x)) %>%
    rename_with(~ gsub("app_order", "tate_order", .x))

  ## Print raw data's number of rows
  message(paste0("We have total ", nrow(temp), " respondents."))
  
  ## Make sure that there are no variations in the _module_rnd
  temp_rnd <- temp %>%
    select(
      ends_with("_module_rnd"),
      ends_with("_page_rnd"), 
      ends_with("_col_rnd"), 
      ends_with("order_q_rnd")
    )
  
  for (v in names(temp_rnd)) {
    ## After NAs are dismissed, there should be only one entry
    assert_that(length(setdiff(unique(temp_rnd[[v]]), NA)) < 2)
  }
  
  ## Separate out timing variables away from substantial responses
  timing <- temp %>%
    select(
      response_id,
      matches(yougov_meta %>% paste(collapse = "|")),
      ## Timing
      ends_with("_timing"), 
      ## No module-level, column, or page randomization took place
      ## Randomization was actually recorded in variables such as tate_order_q
      # ends_with("_module_rnd"),
      # ends_with("_page_rnd"),
      # ends_with("_col_rnd"),
      ## This is also empty
      # ends_width("order_q_rnd"),
      ## This is the important bit for robustness checks
      ends_with("_order_q"),
      ends_with("berinsky_rnd"),
      ## This indicates whether they were exposed to no-context 3 options vs. 4
      ## and such; it's pretty straightforward from NA values
      ## and besides, the naming and values are very uninformative
      # ends_with("_ranking")
      ## I'm not even sure what these are
      ## rand_5_ranking_rnd for example was randomized
      # starts_with("p_implicit_page_")
      # ends_with("_ranking_rnd)
      starts_with("rand_fact"),
      ends_with("_order")
    ) %>%
    mutate(
      duration_in_seconds = as.numeric(ms(endtime)),
      duration_in_minutes = duration_in_seconds / 60
    )
  
  main <- temp %>% 
    select(
      !ends_with("_rnd"),
      ends_with("_row_rnd")
    ) %>%
    select(
      -ends_with("_timing"),
      # -ends_with("_module_rnd"),
      # -ends_with("_page_rnd"),
      # -ends_with("_col_rnd"),
      # -ends_with("berinsky_rnd"),
      # -ends_with("_ranking_rnd"),
      -ends_with("_order_q"),
      -ends_with("_ranking"),
      -matches(yougov_meta %>% paste(collapse = "|")),
      -starts_with("p_implicit_page_"),
      -starts_with("rand_fact"),
      -ends_with("_order")
    )
  
  ## Delete further unnecessary variables (recheck with real data)
  main <- main %>%
    ## age and age4 already exist
    select(-contains("birthyr")) %>%
    ## gender3 is sufficient
    select(-contains("gender3_other"), -gender) %>%
    ## pid3 and pid7 is sufficient
    select(-pid3_t, -pid3_why, -pid7others, -pid7text) %>%
    ## Other text variables
    select(-religpew_t, -presvote20post_t, -housevote22post_t) %>%
    ## hobbies/socialize are not substantial questions but cushions
    select(-contains("hobbies"), -contains("socialize"))
  
  ## Berinsky and Ternovski screeners
  main <- main %>%
    mutate(
      across(
        matches("berinsky|ternovski"),
        ~ case_when(
          .x == "selected" ~ 1,
          .x == "not selected" ~ 0,
          TRUE ~ NA_real_
        )
      )
    )
  
  ## Unite columns
  main <- main %>%
    unite(
      col = "ternovski", sep = "|",
      ternovski_1, ternovski_2, ternovski_3, ternovski_4, ternovski_5
    )
  
  ## Unite rankings into a permutation pattern, a single variable
  main <- main %>%
    unite_ranking()
  
  ## Turn _row_rnd (Qualtrics equivalent of _do) into something more legible
  var_list <- main %>% select(ends_with("_row_rnd")) %>% names()
  
  ## YouGov has a particular style of showing randomized item order
  ## Make it consistent with existing code tailored to Qualtrics
  ## e.g., "randomize(5, [2,0,1])" ---> 312
  ## chatGPT output, spot checked
  item_order_transform <- function(input_string) {
    ## Extract substring between square brackets
    substring <- gsub(".*\\[([^]]+)\\].*", "\\1", input_string)
    
    ## Split the substring by comma
    numbers <- strsplit(substring, ",")[[1]]
    
    ## Increment each number by 1, remove spaces, and concatenate
    result <- ""
    for (number in numbers) {
      incremented_number <- as.numeric(trimws(number)) + 1
      result <- paste0(result, incremented_number)
    }
    
    ## Return NA if string NA
    if (result == "NA") {
      result <- NA
    }
    
    ## Return the final string
    return(result)
  }
  
  ## Transform the item order variables
  main <- main %>%
    rowwise() %>%
    mutate(across(ends_with("_row_rnd"), ~ item_order_transform(.x))) %>%
    ungroup()
  
  ## Remember, we still don't have the *observed* ranking of the respondent
  ## unless the randomization has been given a 1234... natural order
  
  ## So if the true order respondent gave out  is 3-2-1
  ## but the order provided is                    3-1-2
  ## the *observed* ranking is                    1-3-2
  ## unite_ranking() will give 321 for this respondent.
  
  ## Simiarly, if the true order respondent gave out is 6-3-2-7-1-4-5
  ## but the order provided is                          6-1-5-2-7-3-4
  ## the true order respondent provided is              4-6-1-3-5-2-7

  ## Recover recorded rankings
  for (v in var_list) {
    main <- recover_recorded_rankings(v, gsub("_row_rnd", "", v), df = main)
  }
  
  ## Create binary indicators for anchor question/attention check/repeat q fails
  main <- main %>%
    ## 1 indicates failure; 0 indicates passing the test
    mutate(
      ## Attention checks
      ternovski_fail = case_when(
        ternovski != "1|1|0|0|0" ~ 1,
        ternovski == "1|1|0|0|0" ~ 0
      ),
      berinsky_fail = case_when(
        berinsky != "000100000001000000" ~ 1,
        berinsky == "000100000001000000" ~ 0
      ),
      ## Random responses
      random_tate = case_when(
        anc_tate != "123" ~ 1,
        anc_tate == "123" ~ 0
      ),
      random_identity = case_when(
        anc_identity != "1234" ~ 1,
        anc_identity == "1234" ~ 0
      ),
      random_id_alphabet = case_when(
        anc_id_alphabet != "1234" ~ 1,
        anc_id_alphabet == "1234" ~ 0
      ),
      random_id_exact = case_when(
        ## The reason this particular string isn't 1234 is because
        ## teacher and relative were entered in the wrong order in the fielding
        anc_id_alphabet != "1243" ~ 1,
        anc_id_alphabet == "1243" ~ 0
      ),
      random_polar = case_when(
        anc_polar != "12345" ~ 1,
        anc_polar == "12345" ~ 0
      ),
      random_esystems = case_when(
        anc_esystems != "123456" ~ 1,
        anc_esystems == "123456" ~ 0
      ),
      random_es_alphabet = case_when(
        anc_es_alphabet != "123456" ~ 1,
        anc_es_alphabet == "123456" ~ 0
      ),
      random_es_temporal = case_when(
        anc_es_temporal != "123456" ~ 1,
        anc_es_temporal == "123456" ~ 0
      ),
      ## Repeat questions
      repeat_tate = case_when(
        app_tate == app_tate_repeat ~ 0,
        app_tate == app_tate_repeat ~ 1
      ),
      repeat_identity = case_when(
        app_identity == app_identity_repeat ~ 0,
        app_identity == app_identity_repeat ~ 1
      ),
      repeat_polar = case_when(
        app_polar == app_polar_repeat ~ 0,
        app_polar == app_polar_repeat ~ 1
      ),
      repeat_esystems = case_when(
        app_esystems == app_esystems_repeat ~ 0,
        app_esystems == app_esystems_repeat ~ 1
      )
      ## Truncated rankings not yet propagated to main coding
    )
  
  ## Only complete responses
  timing <- timing %>%
    filter(respondent_status == "Complete")

  main <- main %>%
    filter(response_id %in% timing$response_id)
  
  ## After filtering
  message(paste0("We have total ", nrow(main), " respondents after filtering."))
  
  return(list(main = main, timing = timing, raw = df_raw))
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
unite_ranking <- function(x, remove = TRUE) {
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
                  target,
                  anc_correct, # string for correctness indicator
                  n_bootstrap = 250,
                  asymptotics = FALSE) {
  # Prepare
  N <- dim(data)[1] # Number of obs
  J <- length(rank_q)
  target_nmb <- which(rank_q %in% target) # -th position

  # Storages for bootstrapping
  list_prop <- list()
  list_avg <- list()

  # Sampling with replacement

  set.seed(123456)

  for (i in 1:n_bootstrap) {
    index <- sample(1:nrow(data), size = dim(data)[1], replace = TRUE) # RESAMPLE WITH REPLACEMENT
    bs.dat <- data[index, ] # BOOTSTRAPPED DATA


    # Anchor ranking only
    loc_anc <- bs.dat %>% select(contains(paste0("anc_")) &
      matches("[[:digit:]]"))

    # Main ranking only
    loc_app <- bs.dat %>% select(all_of(rank_q))


    # Equation (8) -- proportion of random answers
    Corr <- bs.dat[anc_correct] %>% pull()
    adjust <- mean(Corr) - 1 / factorial(J)
    normalizer <- (1 - 1 / factorial(J))
    p_non_random <- adjust / normalizer


    # Equation (9) -- distribution of random answers

    # Directly apply bias correction to our QOIs

    A_avg <- loc_anc %>%
      summarise(across(everything(), ~ mean(as.numeric(.x))))



    if (asymptotics == FALSE) {
      # We will fully learn from data
      ## including, noise via sampling variability

      # A <- f_anchor$Freq / N # -- empirical pmf of rankings in anchor
      B <- p_non_random # -- estimated proportion of non-random
      C <- data.frame(t(1:J)) # -- true pmf of rankings in anchor

      f_random_avg <- (A_avg - (B * C)) / (1 - B)
    } else {
      # We use theory to get f_random_avg
      ## We know that the entire PMF will follow a uniform distribution
      ## f_random <- rep(1/factorial(J), factorial(J))
      ## We then compute average ranks based on the asymptotic distribution

      f_random_avg <- (1 + J) / 2 # This is true as n --> infinity
    }


    # Redefine B
    B <- p_non_random # -- estimated proportion of non-random

    # Equation (10) -- distribution of error-free rankings

    D_avg <- loc_app %>%
      summarise(across(everything(), ~ mean(as.numeric(.x))))

    E <- f_random_avg # -- estimated pmf of errors in the anchor

    imp_avg <- (D_avg - ((1 - B) * E)) / B

    # This method produces outside-the-bound values
    # --> Yuki is correcting this temporarily

    bound <- function(x) {
      ifelse(x < 1, 1, ifelse(x > J, J, x))
    }

    imp_avg <- imp_avg %>%
      mutate(across(everything(), ~ bound(.x)))

    list_prop[[i]] <- p_non_random
    list_avg[[i]] <- imp_avg
  }


  out_prop <- do.call(rbind.data.frame, list_prop)

  # Improved average ranks
  out_avg <- do.call(rbind.data.frame, list_avg) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise(
      "est" = mean(value),
      "low" = quantile(value, prob = 0.025),
      "up" = quantile(value, prob = 0.975)
    ) %>%
    ungroup() %>%
    mutate(imp = "improved")


  # Raw average ranks
  raw_avg <- data %>%
    select(all_of(rank_q)) %>%
    pivot_longer(everything())


  raw_est <- list()

  for (j in 1:J) {
    dt_j <- raw_avg %>%
      filter(name == rank_q[j])

    raw_ols <- lm_robust(as.numeric(value) ~ 1, dt_j) %>%
      tidy() %>%
      mutate(
        item = rank_q[j],
        imp = "raw data"
      ) %>%
      select(
        est = estimate,
        low = conf.low,
        up = conf.high,
        name = item,
        imp
      )

    raw_est[[j]] <- raw_ols
  }


  raw_est <- do.call(rbind.data.frame, raw_est)
  raw_est


  # Combine both estimates
  out <- rbind(out_avg, raw_est) %>%
    arrange(name)


  return(out) # return the list
}

## Not perfectly do-not-repeat-yourself but will return later
pattern_compare_pass_fail <- function(main, v, y_upper = .75, label = NULL,
                                      observed = TRUE) {
  if (observed) {
    sfx <- "_observed"
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

viz_avg <- function(data) {
  data %>%
    mutate(name = fct_reorder(name, est)) %>%
    ggplot(., aes(x = fct_rev(name), y = est, color = imp)) +
    geom_point(
      aes(shape = imp),
      size = 2, alpha = 0.75,
      position = position_dodge(width = 0.6)
    ) +
    # Reorder by point estimate
    geom_linerange(
      aes(ymin = low, ymax = up),
      alpha = 0.75,
      lwd = 1, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c("#b0015a", "#999999")) +
    theme_bw() +
    coord_flip() +
    xlab("") +
    ylab("") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 0),
      plot.title = element_text(face = "bold")
    )
}
