source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Subset and wrangle data that we need =========================================
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

## Reference set: (party, religion, gender, race)
identity_data <- main %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity, anc_correct_id_alphabet, anc_correct_id_exact,
    weight
  )

# Main anchor question =========================================================
## Note that the functions now require whether the respondent made a 
## correct answer to the anchor question in a binary format, 
## *outside* the function

## Direct bias correction ------------------------------------------------------
main_direct <- imprr_direct(
  data = identity_data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = identity_data$weight,
  n_bootstrap = 1000
)

## Inverse probability weighting -----------------------------------------------
main_ipw <- imprr_weights(
  data = identity_data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)

# Alphabet anchor ==============================================================
temp <- identity_data %>% filter(!is.na(anc_correct_id_alphabet))

## Direct bias correction ------------------------------------------------------
alphabet_direct <- imprr_direct(
  data = temp,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_id_alphabet",
  weight = temp$weight,
  n_bootstrap = 1000
)

# Inverse probability weighting
alphabet_ipw <- imprr_weights(
  data = temp,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_id_alphabet"
)

# Exact anchor =================================================================
temp <- identity_data %>% filter(!is.na(anc_correct_id_exact))

## Direct bias correction ------------------------------------------------------
exact_direct <- imprr_direct(
  data = temp,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_id_exact",
  weight = temp$weight,
  n_bootstrap = 1000
)

# Inverse probability weighting
exact_ipw <- imprr_weights(
  data = temp,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_id_exact"
)

# Save results for reuse =======================================================
main_ipw_weighted$weights <- main_ipw_weighted$weights %>%
  rename(alt_w = w)

## Downsized data
dt <- main %>%
  mutate(
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race_ethnicity = app_identity_4,
    ranking = app_identity
  ) %>%
  left_join(main_ipw_weighted$weights, by = "ranking") %>%
  left_join(main_ipw$weights, by = "ranking") %>%
  mutate(w_multiplied = weight * w) %>%
  select(
    party, religion, gender, race_ethnicity, race, app_identity, 
    ranking, weight, w, alt_w, w_multiplied,
    race4, ideo7, pid7, educ, age, partisan, region, male
  )

save(
  list = c(
    "main_direct", "main_ipw", 
    "alphabet_direct", "alphabet_ipw",
    "exact_direct", "exact_ipw",
    "identity_data", "dt"
  ),
  file = here("data", "tidy", "bias_correction.Rda")
)
