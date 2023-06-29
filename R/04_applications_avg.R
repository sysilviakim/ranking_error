source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

# Setup ========================================================================
prep_list <- root_var %>%
  imap(
    ~ {
      full <- main %>%
        select(
          contains(paste0("app_", .y)),
          matches(paste0("anc_", .y)),
          matches(paste0("anc_correct_", .y, "$")),
          matches(paste0("anc_", .y, "_recorded"))
        ) %>%
        select(-contains("row_rnd")) %>%
        select(-matches("^inputstate$"))
      crosswalk <- option_crosswalk[option_crosswalk %in% names(full)]
      full <- full %>% rename(!!!crosswalk)

      rational <- full %>%
        filter(
          !!as.name(paste0("app_", .y)) ==
            !!as.name(paste0("app_", .y, "_repeat"))
        )

      full <- full %>%
        select(-contains("repeat"))
      
      return(
        list(
          full = full, rational = rational, 
          labels = names(crosswalk),
          full_N = nrow(full), rational_N = nrow(rational)
        )
      )
    }
  )

# Average rankings =============================================================
## Quick summary
prep_list %>%
  imap(
    ~ avg_rank(.x$full, paste0("app_", .y)) %>%
      map_dbl("mean") %>%
      round(., digits = 1)
  )

# $tate
# 1st 2nd 3rd 
# 1.7 2.0 2.2 
# 
# $identity
# 1st 2nd 3rd 4th 
# 3.0 2.6 1.9 2.5 
# 
# $polar
# 1st 2nd 3rd 4th 5th 
# 2.3 2.6 2.9 3.1 4.2 
# 
# $esystems
# 1st 2nd 3rd 4th 5th 6th 
# 2.7 2.7 4.1 4.1 4.3 3.1

prep_list %>%
  imap(
    ~ avg_rank(.x$full, paste0("app_", .y)) %>%
      map(~ .x) %>% 
      bind_rows(.id = "reference") %>%
      rowwise() %>%
      mutate(varname = paste0("app_", .y, "_", substr(reference, 1, 1))) %>%
      mutate(
        crosswalk = names(option_crosswalk)[which(option_crosswalk == varname)]
      ) %>%
      ungroup() %>%
      select(reference, varname, crosswalk, everything())
  )

