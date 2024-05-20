# Setup ========================================================================
source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

# Four different applications ==================================================
data <- prep_list$identity$full
main_q <- "app_identity"
anchor_q <- "anc_identity"
anc_correct <- "anc_correct_identity"
anc_correct_pattern <- NULL
main_labels <- prep_list$identity$labels
asymptotics <- TRUE
n_bootstrap <- 200
seed <- 123456

# Apply the bias correction to the PMF
temp <- imprr(
  data = data,
  main_q = main_q,
  anchor_q,
  anc_correct,
  anc_correct_pattern = NULL,
  main_labels,
  n_bootstrap = 250,
  asymptotics = TRUE,
  seed = 123456
)

write_csv(temp, here::here("data/tidy", "temp_weight.csv"))
