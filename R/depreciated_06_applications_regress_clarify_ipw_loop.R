source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
load(here("data", "tidy", "bias_correction.Rda"))
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

identity_data <- main %>%
  mutate(ranking = app_identity) %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    ranking, weight, race4, ideo7, pid7, educ, age, partisan, region, male,
    anc_correct_identity
  ) %>%
  drop_na()

library(clarify)
library(mlogit)

# 1. Get data, get bias-correction weights =====================================
## Now already performed within 03_bias_correction.R
.fix_pid <- 4 # Moderate
.fix_male <- 1 # Male
.fix_age <- 40 # close to the average of 52.76
.fix_edu <- 3 # some college

n_bootstrap <- 200
set.seed(123)
seed_list <- sample(
  1:max(n_bootstrap * 10, 10000), n_bootstrap,
  replace = FALSE
)
seed_list <- as.list(seed_list)
out_ipw <- list()


for(b in 1:n_bootstrap){

out_ipw[[b]] <-  boot_luce_temp(identity_data, seed_list[[b]])

  
}


out_ipw_df <- data.frame(
  ideology = vector("numeric", n_bootstrap),
  mean = vector("numeric", n_bootstrap),
  ranking = vector("character", n_bootstrap)
)

# Store all results in a data frame
out_ipw_df <- seq(n_bootstrap) %>%
  map(
    ~ data.frame(
      ideology = out_ipw[[.x]]$ideology,
      mean = out_ipw[[.x]]$mean,
      ranking = out_ipw[[.x]]$ranking
    )
  ) %>%
  bind_rows()
out_ipw_df

PL_ipw_loop <- out_ipw_df %>%
  group_by(ideology, ranking) %>%
  summarize(
    low = quantile(mean, prob = 0.025),
    up = quantile(mean, prob = 0.975),
    mean = mean(mean)
  ) %>%
  mutate(results = "bias-corrected") %>%
  select(ideology, mean, low, up, results, ranking) %>%
  arrange(ranking)

## Checked that the old file Yuki committed is reproducible? Not quite
## PL_ipw_old <- loadRData(here("data", "tidy", "PL_ipw_old.Rda"))
## Then change n_bootstrap to 1,000
save(PL_ipw_loop, file = here("data", "tidy", "PL_ipw_loop.Rda"))
