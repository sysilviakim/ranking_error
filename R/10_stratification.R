source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Subset and wrangle data that we need =========================================
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

## Reference set: (party, religion, gender, race)
identity_data <- main %>%
  mutate(pid_recode = case_when(pid3 == 1 ~ "Democrat",
                                pid3 == 2 ~ "Republican",
                                TRUE ~ "Others")) %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity, anc_correct_id_alphabet, anc_correct_id_exact,
    weight,
    pid_recode
  )


## Stratify by partisanship
data_dem <- identity_data %>% filter(pid_recode == "Democrat")
data_rep <- identity_data %>% filter(pid_recode == "Republican")
data_oth <- identity_data %>% filter(pid_recode == "Others")

# Apply bias correction ========================================================
## Democrat --------------------------------------------------------------------
direct_dem <- imprr_direct(
  data = data_dem,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data_dem$weight,
  n_bootstrap = 1000
)

## Republican ------------------------------------------------------------------
direct_rep <- imprr_direct(
  data = data_rep,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data_rep$weight,
  n_bootstrap = 1000
)

## Others ----------------------------------------------------------------------
direct_othe <- imprr_direct(
  data = data_oth,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data_oth$weight,
  n_bootstrap = 1000
)

save(
  list = c(
    "direct_dem", "direct_rep", "direct_othe",
    "identity_data"
  ),
  file = here("data", "tidy", "stratification.Rda")
)


# Stratification estimate ======================================================
load(here("data", "tidy", "stratification.Rda"))

avg_rank.w <- direct_dem$qoi %>%
  filter(qoi == "average rank") %>%
  ungroup(qoi) %>%
  mutate(
    item = case_when(
      item == "app_identity_1" ~ "party",
      item == "app_identity_2" ~ "religion",
      item == "app_identity_3" ~ "gender",
      item == "app_identity_4" ~ "race_ethnicity"
    ))

p_X <- prop.table(table(identity_data$pid_recode))


## Stratification estimate

est_dem <- direct_dem$qoi %>% filter(qoi == "average rank") %>% ungroup() %>% select(item, mean)
est_rep <- direct_rep$qoi %>% filter(qoi == "average rank") %>% ungroup() %>% select(item, mean)
est_oth <- direct_othe$qoi %>% filter(qoi == "average rank") %>% ungroup() %>% select(item, mean)


strat <- 
est_dem[1:4, 2] * p_X[1] + 
  est_rep[1:4, 2] * p_X[2] + 
  est_oth[1:4, 2] * p_X[3] 

strat$item <- c("party,", "religion", "gender", "race_ethnicity")

strat

