source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# Identity application =========================================================
## Reducing the options

## Recoding --------------------------------------------------------------------
## Suppose we only asked party (1), religion (3), gender (4), and race (6)
main <- main %>%
  mutate(app_identity_short = gsub("2|5|7", "", app_identity)) %>%
  ## Recode party 1, religion 2, gender 3, race 4
  mutate(
    app_identity_short = gsub("3", "2", app_identity_short),
    app_identity_short = gsub("4", "3", app_identity_short),
    app_identity_short = gsub("6", "4", app_identity_short)
  )

tab_identity <- table(main$app_identity_short)
chisq_power(permn_augment(tab_identity))

## Avg. ranks ------------------------------------------------------------------
### All responses
avg_rank(
  main %>% filter(!grepl("9", app_identity_short)),
  "app_identity_short"
)

# (1) party ---> 2.90
# (2) religion ---> 2.43
# (3) gender ---> 2.28
# (4) race ---> 2.40

### Non-geometric
avg_rank(
  main %>% filter(!grepl("9", app_identity_short)) %>% filter(ns_identity == 0),
  "app_identity_short"
)

# (1) party ---> 3.19
# (2) religion ---> 2.30
# (3) gender ---> 2.22
# (4) race ---> 2.30

## By party? -------------------------------------------------------------------
main %>%
  group_by(pid3alt) %>%
  group_split(.keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$pid3alt[1]) %>% unlist()) %>%
  map(~ avg_rank(.x, "app_identity_short"))

# (1) party ---> Dem 2.86 Rep 3.05
# (2) religion ---> Dem 2.33 Rep 2.37
# (3) gender ---> Dem 2.29 Rep 2.50
# (4) race ---> Dem 2.52 Rep 2.08

## What if raw?
main %>%
  group_by(pid3alt) %>%
  group_split(.keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$pid3alt[1]) %>% unlist()) %>%
  map(~ avg_rank(.x, "app_identity"))

# $Dem
# # A tibble: 1 × 7
# `1st` `2nd` `3rd` `4th` `5th` `6th` `7th`
# <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1  4.52  4.10  4.02  3.88  2.98  4.43  4.07
# 
# $Ind
# # A tibble: 1 × 7
# `1st` `2nd` `3rd` `4th` `5th` `6th` `7th`
# <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1  5.22  3.44  4.06  3.67  3.28  4.17  4.17
# 
# $Rep
# # A tibble: 1 × 7
# `1st` `2nd` `3rd` `4th` `5th` `6th` `7th`
# <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#   1  5.32  4.47  4.21  3.68  2.76  4.66  2.89

# Polarization application =====================================================
## Suppose we didn't ask about electoral systems ()



