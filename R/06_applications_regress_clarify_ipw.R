source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
load(here("data", "tidy", "bias_correction.Rda"))
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

# ## Reference set: (party, religion, gender, race)
# identity_data <- main %>%
#   mutate(ranking = app_identity) %>%
#   select(
#     app_identity_1, app_identity_2, app_identity_3, app_identity_4,
#     anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
#     anc_correct_identity, anc_correct_id_alphabet, anc_correct_id_exact,
#     weight, partisan, race4, region, educ4, age, pid7, ideo7, ranking
#   )

identity_data <- main %>%
  mutate(ranking = app_identity) %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    ranking, weight,
    race4, ideo7, pid7, educ, age, partisan, region, male,
    anc_correct_identity
  ) %>%
  drop_na()

library(clarify)
library(mlogit)
library(future.apply)

# 1. Get data, get bias-correction weights =====================================
## Now already performed within 03_bias_correction.R
## Sanity check

# Functionalize bootstrapping with Plackett-Luce
boot_luce_temp <- function(identity_data,
                           boot_seed) {
  index <- 
    sample(1:nrow(identity_data), size = nrow(identity_data), replace = TRUE)
  bootstrap_dat <- identity_data[index, ]

  boot_ipw <- imprr_weights(
    data = bootstrap_dat,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = boot_seed
  )

  # Bootstrap data with estimated weights
  bootstrap_dat <- bootstrap_dat %>%
    left_join(boot_ipw$weights, by = "ranking") %>%
    mutate(w_multiplied = weight * w)


  # 2. Data processing =========================================================
  # Reference set: (party, religion, gender, race)
  # Downsize data
  dt <- bootstrap_dat %>%
    mutate(
      id = 1:nrow(bootstrap_dat),
      race = as.factor(race4),
      region = as.factor(region)
    ) %>%
    rename(
      ch.party = app_identity_1,
      ch.religion = app_identity_2,
      ch.gender = app_identity_3,
      ch.race = app_identity_4
    ) %>%
    ## Use race4 instead of race6 that was previously used
    drop_na()

  # 3. Create indexed data =====================================================
  # Transform into mlogit format
  dt <- as.data.frame(dt)

  ## Takes about 2 seconds
  mdat <- dfidx::dfidx(
    dt,
    shape = "wide",
    choice = "ch",
    varying = 1:4, # 1:J, J = # items
    ranked = TRUE
  )
  # Check
  head(mdat)

  # print(b)
  # mdat$ch # logical: TRUE if id2 was ranked idx1-th, by unit id1
  # mdat$id # respondent id
  # mdat$idx # position id (1st, 2nd, 3rd, 4th = depressed)
  # mdat$ideo7 # explanatory variable

  # 4. Run Rank-order logit model ==============================================

  # Estimating parameters (with weight)
  m2 <- mlogit(
    ch ~ 1 | ideo7 + pid7 + partisan + male + age + race4 + educ + region,
    mdat, # Data
    reflevel = "gender", # Base category
    weight = w_multiplied # Survey weights X bias-correction weights
  )

  # Raw result
  summary(m2)

  # 5. Get predicted probabilities =============================================
  ## 5.1. Gender > Race > Party > Religion =====================================
  # intercept
  # partisan --> Independent
  # race --> White
  # region --> 1 (Northeast)
  # ideo7 --> we vary this
  .fix_pid <- 4 # Moderate
  .fix_male <- 1 # Male
  .fix_age <- 40 # close to the average of 52.76
  .fix_edu <- 3 # some college

  # Generate 1000 sets of parameters (parametric bootstrap)
  ## First, reusable template
  p_template <- data.frame(
    ideology = 1:7,
    mean = NA,
    low = NA,
    up = NA
  )

  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  p_qoi2 <- regress_clarify_temp(v, 7, p_template, type = 1)

  ggdt1 <- rbind(p_qoi2) %>%
    mutate(ranking = "Pr(gender > race > party > religion)")

  ## 5.2. Party > Gender > Race > Religion =====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  p_qoi2 <- regress_clarify_temp(v, 7, p_template, type = 2)

  ggdt2 <- rbind(p_qoi2) %>%
    mutate(ranking = "Pr(party > gender > race > religion)")

  ## 5.3. Gender > Race > Party > Religion =====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  p_qoi2 <- regress_clarify_temp(v, 7, p_template, type = 3)

  ggdt3 <- rbind(p_qoi2) %>%
    mutate(ranking = "Pr(gender > race > religion > party)")

  ## 5.4. Religion > Gender > Race > Party  ====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  p_qoi2 <- regress_clarify_temp(v, 7, p_template, type = 4)

  ggdt4 <- rbind(p_qoi2) %>%
    mutate(ranking = "Pr(religion > gender > race > party)")


  # Tie everything together
  ggdt_ipw <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)

  return(ggdt_ipw)
}

seeds <- 123
n_bootstrap <- 200
set.seed(seeds)
seed_list <- sample(1:max(n_bootstrap * 10, 10000), n_bootstrap,
  replace = FALSE
)
seed_list <- as.list(seed_list)

out_ipw <- list()

# using multiple CPU cores 
## (https://stackoverflow.com/questions/70213799/)
plan(multisession) 
out_ipw <- future_sapply(seed_list,
  boot_luce_temp,
  identity_data = identity_data
)

out_ipw_df <- data.frame(
  ideology = as.numeric(),
  mean = as.numeric(),
  ranking = as.character()
)

# Store all results in a data frame
for (b in 1:n_bootstrap) {
  temp <- data.frame(
    ideology = out_ipw[, b]$ideology,
    mean = out_ipw[, b]$mean,
    ranking = out_ipw[, b]$ranking
  )

  out_ipw_df <- rbind(out_ipw_df, temp)
}

out_ipw_df

PL_ipw <- out_ipw_df %>%
  group_by(ideology, ranking) %>%
  summarize(
    low = quantile(mean, prob = 0.025),
    up = quantile(mean, prob = 0.975),
    mean = mean(mean)
  ) %>%
  mutate(results = "bias-corrected") %>%
  select(ideology, mean, low, up, results, ranking) %>%
  arrange(ranking)

save(PL_ipw, file = here("data", "tidy", "PL_ipw.Rda"))
