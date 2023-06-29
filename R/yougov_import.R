## import and wrangle YouGov data
yougov_import <- function(fname) {
  if (grepl(".csv", fname)) {
    df_raw <- read_csv(here("data", "raw", fname), show_col_types = FALSE)
  } else {
    df_raw <- read_sav(here("data", "raw", fname))
  }
  ## Exported using not the choice text but numeric values
  df_raw <- df_raw %>%
    clean_names() %>%
    mutate(response_id = str_pad(row_number(), width = 4, pad = "0")) %>%
    select(response_id, everything())
  
  ## Metadata
  yougov_meta <- c(
    "version", "exit_status", "respondent_status", "disposition", "last_page",
    "starttime", "endtime", "favor_or_oppose", "follow_up", "hcountry_code", 
    "perc_skipped", "phone_flag", "points", 
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
    assert_that(length(setdiff(unique(temp_rnd[[v]]), c(NA, "NA"))) < 2)
  }
  
  ## Further zero-variance variables
  temp2 <- Filter(
    function(x) case_when(
      all(is.na(x)) ~ FALSE,
      !any(is.na(x)) & var(x, na.rm = TRUE) == 0 ~ FALSE,
      TRUE ~ TRUE
    ), 
    temp
  )
  
  setdiff(names(temp), names(temp2))
  #  [1] "consent"                               "invalidrank"                          
  #  [3] "page_esystems_order_q_timing"          "page_back_timing"                     
  #  [5] "page_birthyr_timing"                   "page_check_scores_timing"             
  #  [7] "page_educ4_timing"                     "page_gender_timing"                   
  #  [9] "page_headers_timing"                   "page_identity_order_q_timing"         
  # [11] "page_immigrant_screen_out_page_timing" "page_mobilescreenpage_timing"         
  # [13] "page_p_empty_timing"                   "page_polar_order_q_timing"            
  # [15] "page_race4_timing"                     "page_rand_1_ranking_timing"           
  # [17] "page_rand_2_ranking_timing"            "page_rand_3_ranking_timing"           
  # [19] "page_rand_4_ranking_timing"            "page_rand_5_ranking_timing"           
  # [21] "page_region_timing"                    "page_tate_order_q_timing"             
  # [23] "page_three_strikes_timing"             "page_votereg2_timing"                 
  # [25] "page_welcomeset_timing"                "phone_flag"    
  
  ## Separate out timing variables away from substantial responses
  timing <- temp2 %>%
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
      ends_with("_order"),
      ## Whether Dem was presented first or Rep
      matches("^dem_rep_can$"),
      matches("^lib_con$")
    )
  
  if ("starttime" %in% names(timing)) {
    timing <- timing %>%
      mutate(duration_in_minutes = endtime - starttime)
  } else {
    timing <- timing %>%
      mutate(duration_in_minutes = as.numeric(ms(endtime)) / 60)
  }
  
  main <- temp2 %>% 
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
      -ends_with("_order"),
      -matches("^dem_rep_can$"),
      -matches("^lib_con$")
    )
  
  ## Delete further unnecessary variables (recheck with real data)
  main <- main %>%
    ## age and age4 already exist
    select(-contains("birthyr")) %>%
    ## gender3 is sufficient
    select(-contains("gender3_other"), -matches("^gender$")) %>%
    ## pid3 and pid7 is sufficient
    select(-pid3_t, -matches("^pid3_why$"), -pid7others, -pid7text) %>%
    ## Other text variables
    select(
      -matches("^religpew_t$"), 
      -matches("^presvote20post_t$"), 
      -matches("^housevote22post_t$")
    ) %>%
    ## hobbies/socialize are not substantial questions but cushions
    select(-contains("hobbies"), -contains("socialize"))
  
  ## Berinsky and Ternovski screeners
  if (any(grepl("selected", main$berinsky_1))) {
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
  }
  
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
  ## rowwise will slow things down; must find way to optimize
  message("Stripping item orders of brackets and other unnecessary strings.")
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
  message("Recovering recorded rankings.")
  for (v in var_list) {
    main <- recover_recorded_rankings(v, gsub("_row_rnd", "", v), df = main)
  }
  
  ## Create binary indicators for anchor question/attention check/repeat q fails
  main <- main %>%
    ## 1 indicates failure; 0 indicates passing the test
    mutate(
      ## Attention checks
      ternovski_fail = case_when(
        ## Test link and actual data are different
        (ternovski != "1|1|0|0|0") & (ternovski != "1|1|2|2|2") ~ 1,
        (ternovski == "1|1|0|0|0") | (ternovski == "1|1|2|2|2") ~ 0
      ),
      berinsky_fail = case_when(
        (berinsky != "000100000001000000") & 
          (berinsky != "222122222221222222") ~ 1,
        (berinsky == "000100000001000000") | 
          (berinsky == "222122222221222222") ~ 0
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
        anc_id_exact != "1243" ~ 1,
        anc_id_exact == "1243" ~ 0
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
      ## Perfectly collinear variables, just renamed/recoded for easy access
      anc_correct_tate = 1 - random_tate,
      anc_correct_identity = 1 - random_identity,
      anc_correct_id_alphabet = 1 - random_id_alphabet,
      anc_correct_id_exact = 1 - random_id_exact,
      anc_correct_polar = 1 - random_polar,
      anc_correct_esystems = 1 - random_esystems,
      anc_correct_es_alphabet = 1 - random_es_alphabet,
      anc_correct_es_temporal = 1 - random_es_temporal,
      ## Repeat questions
      repeat_tate = case_when(
        app_tate == app_tate_repeat ~ 0,
        app_tate != app_tate_repeat ~ 1
      ),
      repeat_identity = case_when(
        app_identity == app_identity_repeat ~ 0,
        app_identity != app_identity_repeat ~ 1
      ),
      repeat_polar = case_when(
        app_polar == app_polar_repeat ~ 0,
        app_polar != app_polar_repeat ~ 1
      ),
      repeat_esystems = case_when(
        app_esystems == app_esystems_repeat ~ 0,
        app_esystems != app_esystems_repeat ~ 1
      )
      ## Truncated rankings not yet propagated to main coding
    )
  
  ## Other wranglings
  main <- main %>%
    mutate(
      pid3final = case_when(
        pid7 %in% c(1, 2, 3) ~ "Democrat",
        pid7 %in% c(5, 6, 7) ~ "Republican",
        pid7 %in% c(4, 8) ~ "Independent"
      ),
      partisan = case_when(
        pid7 %in% c(1, 7) ~ "Strong Partisan",
        pid7 %in% c(2, 3, 5, 6) ~ "Weak Partisan",
        pid7 %in% c(4, 8) ~ "Independent"
      ),
      race4labeled = case_when(
        race4 == 1 ~ "White",
        race4 == 2 ~ "Black",
        race4 == 3 ~ "Hispanic",
        race4 == 4 ~ "Other race"
      )
    )
  
  ## Only complete responses
  if ("respondent_status" %in% names(timing)) {
    timing <- timing %>%
      filter(respondent_status == "Complete")
    main <- main %>%
      filter(response_id %in% timing$response_id)
  }
  
  ## After filtering
  message(paste0("We have total ", nrow(main), " respondents after filtering."))
  
  return(list(main = main, timing = timing, raw = df_raw))
}
