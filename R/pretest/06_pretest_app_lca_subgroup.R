source(here::here("R", "pretest", "04_pretest_applications.R"))
library(poLCA)

# Application: LCA =============================================================
## Tate ------------------------------------------------------------------------
temp <- bind_cols(
  main,
  avg_rank(main, "app_tate_1993", split_only = TRUE) %>%
    mutate(across(everything(), ~ as.factor(.x)))
)

f <- cbind(`1st`, `2nd`, `3rd`) ~ pid3alt
nes1 <- poLCA(f, temp, nclass = 1)
nes2 <- poLCA(f, temp, nclass = 2)
nes3 <- poLCA(f, temp, nclass = 3)

## Electoral systems -----------------------------------------------------------
temp <- bind_cols(
  main,
  avg_rank(main, "app_e_systems", split_only = TRUE) %>%
    mutate(across(everything(), ~ as.factor(.x)))
)

f <- cbind(`1st`, `2nd`, `3rd`, `4th`, `5th`, `6th`, `7th`) ~ pid3alt
nes1 <- poLCA(f, temp, nclass = 1)
## nes2 <- poLCA(f, temp, nclass = 2)

## Identity --------------------------------------------------------------------
temp <- bind_cols(
  main,
  avg_rank(main, "app_identity", split_only = TRUE) %>%
    mutate(across(everything(), ~ as.factor(.x)))
)

f <- cbind(`1st`, `2nd`, `3rd`, `4th`, `5th`, `6th`, `7th`) ~ pid3alt
nes1 <- poLCA(f, temp, nclass = 1)
nes2 <- poLCA(f, temp, nclass = 2)
## nes3 <- poLCA(f, temp, nclass = 3)

## Polarization ----------------------------------------------------------------
temp <- bind_cols(
  main,
  avg_rank(main, "app_polar", split_only = TRUE) %>%
    mutate(across(everything(), ~ as.factor(.x)))
)

f <- cbind(`1st`, `2nd`, `3rd`, `4th`, `5th`, `6th`, `7th`, `8th`) ~ pid3alt
nes1 <- poLCA(f, temp, nclass = 1)
## nes2 <- poLCA(f, temp, nclass = 2)
## nes3 <- poLCA(f, temp, nclass = 3)

# Application: by party ========================================================
## Tate ------------------------------------------------------------------------
avg_rank(
  main %>% filter(!grepl("9", app_tate_1993) & ns_polar == 0 & pid3alt == "Dem"),
  "app_tate_1993"
)

avg_rank(
  main %>% filter(!grepl("9", app_tate_1993) & ns_polar == 0 & pid3alt == "Rep"),
  "app_tate_1993"
)

## Electoral systems -----------------------------------------------------------
avg_rank(
  main %>% filter(!grepl("9", app_e_systems) & ns_polar == 0 & pid3alt == "Dem"),
  "app_e_systems"
)

avg_rank(
  main %>% filter(!grepl("9", app_e_systems) & ns_polar == 0 & pid3alt == "Rep"),
  "app_e_systems"
)

## Identity --------------------------------------------------------------------
avg_rank(
  main %>% filter(!grepl("9", app_identity) & ns_polar == 0 & pid3alt == "Dem"),
  "app_identity"
)

avg_rank(
  main %>% filter(!grepl("9", app_identity) & ns_polar == 0 & pid3alt == "Rep"),
  "app_identity"
)

## Polarization ----------------------------------------------------------------
avg_rank(
  main %>% filter(!grepl("9", app_polar) & ns_polar == 0 & pid3alt == "Dem"),
  "app_polar"
)

avg_rank(
  main %>% filter(!grepl("9", app_polar) & ns_polar == 0 & pid3alt == "Rep"),
  "app_polar"
)
