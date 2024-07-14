source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
load(here("data", "tidy", "bias_correction.Rda"))
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

library(clarify)
library(mlogit)

# 1. Get data, get bias-correction weights =====================================
## Now already performed within 03_bias_correction.R
## Sanity check
plot(
  dt$weight, dt$w_multiplied,
  xlim = c(0, 6), ylim = c(0, 6)
)
abline(a = 0, b = 1, col = "firebrick4", lty = 2)

# 2. Data processing ===========================================================
# Reference set: (party, religion, gender, race)
# Downsize data
dt <- dt %>%
  mutate(
    id = 1:nrow(main),
    race = as.factor(race4),
    region = as.factor(region)
  ) %>%
  rename(
    ch.party = party,
    ch.religion = religion,
    ch.gender = gender,
    ch.race = race_ethnicity
  ) %>%
  ## Use race4 instead of race6 that was previously used
  drop_na()

# 3. Create indexed data =======================================================
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

mdat$ch # logical: TRUE if id2 was ranked idx1-th, by unit id1
mdat$id # respondent id
mdat$idx # position id (1st, 2nd, 3rd, 4th = depressed)
mdat$ideo7 # explanatory variable

# 4. Run Rank-order logit model ================================================
# Estimating parameters (no bias correction)
m <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + male + age + race4 + educ + region,
  mdat, # Data
  reflevel = "gender", # Base category
  weight = weight # Survey weights
)

# Raw result
summary(m)

# 5. Get predicted probabilities ===============================================
## 5.1. Gender > Race > Party > Religion =======================================
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

set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 1)
p_qoi$results <- "raw data" # no weight

ggdt1 <- p_qoi %>%
  mutate(ranking = "Pr(gender > race > party > religion)")

## 5.2. Party > Gender > Race > Religion =======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 2)
p_qoi$results <- "raw data" # no weight

ggdt2 <- p_qoi %>%
  mutate(ranking = "Pr(party > gender > race > religion)")

## 5.3. Gender > Race > Party > Religion =======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 3)

p_qoi$results <- "raw data" # no weight

ggdt3 <- p_qoi %>%
  mutate(ranking = "Pr(gender > race > religion > party)")

## 5.4. Religion > Gender > Race > Party  ======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 4)

p_qoi$results <- "raw data" # no weight

ggdt4 <- p_qoi %>%
  mutate(ranking = "Pr(religion > gender > race > party)")

# 6. Visualize the final results  ==============================================
PL_raw <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)

# This will be used in 06_applications_regress_clarify_figure.R
save(PL_raw, file = here("data", "tidy", "PL_raw.Rda"))
