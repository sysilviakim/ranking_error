source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
load(here("data", "tidy", "bias_correction.Rda"))
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

library(clarify)
library(mlogit)

temp_fxn <- function(v, n, p_qoi, type = 1) {
  for (i in 1:n) {
    e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
      v$`pid7:party` * .fix_pid +
      v$`male:party` * .fix_male +
      v$`age:party` * .fix_age +
      v$`educ:party` * .fix_edu)
    e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
      v$`pid7:race` * .fix_pid +
      v$`male:race` * .fix_male +
      v$`age:race` * .fix_age +
      v$`educ:race` * .fix_edu)
    e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
      v$`pid7:religion` * .fix_pid +
      v$`male:religion` * .fix_male +
      v$`age:religion` * .fix_age +
      v$`educ:religion` * .fix_edu)
    e_XB_gen <- 1

    # Here, we want to compute the probability for one unique ranking
    # Prob (party, race, religion, gender)
    # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
    # This is multiplication of three multinomial choices
    if (type == 1) {
      ## Gender > Race > Party > Religion
      p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
        e_XB_party / (e_XB_reli + e_XB_party) *
        e_XB_reli / e_XB_reli
    } else if (type == 2) {
      ## Party > Gender > Race > Religion
      p <- e_XB_party / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_gen / (e_XB_race + e_XB_gen + e_XB_reli) *
        e_XB_race / (e_XB_race + e_XB_reli) *
        e_XB_reli / e_XB_reli
    } else if (type == 3) {
      ## Gender > Race > Religion > Party
      p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
        e_XB_reli / (e_XB_reli + e_XB_party) *
        e_XB_party / e_XB_party
    } else if (type == 4) {
      ## Religion > Gender > Race > Party
      p <- e_XB_reli / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_gen / (e_XB_race + e_XB_gen + e_XB_party) *
        e_XB_race / (e_XB_race + e_XB_party) *
        e_XB_party / e_XB_party
    }

    # we want to generate 24 ps. They should sum up to one.
    p_qoi[i, 2] <- mean(p)
    # if we bootstrap the whole thing, we don't need to save this
    p_qoi[i, 3] <- quantile(p, prob = 0.025)
    p_qoi[i, 4] <- quantile(p, prob = 0.975)
  }
  return(p_qoi)
}

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
    race = as.factor(race4),   # This was "race" instead of race4 (Yuki fixed it on 7/4)
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
# Estimating parameters (no weight)
m <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + male + age + race4 + educ + region,
  mdat, # Data
  reflevel = "gender", # Base category
  weight = weight # Survey weights
)

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
p_qoi <- temp_fxn(v, 7, p_template, type = 1)

p_qoi$results <- "raw data" # no weight

ggdt1 <- rbind(p_qoi) %>%
  mutate(ranking = "Pr(gender > race > party > religion)")

## 5.2. Party > Gender > Race > Religion =======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- temp_fxn(v, 7, p_template, type = 2)

p_qoi$results <- "raw data" # no weight

ggdt2 <- rbind(p_qoi) %>%
  mutate(ranking = "Pr(party > gender > race > religion)")

## 5.3. Gender > Race > Party > Religion =======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- temp_fxn(v, 7, p_template, type = 3)

p_qoi$results <- "raw data" # no weight

ggdt3 <- rbind(p_qoi) %>%
  mutate(ranking = "Pr(gender > race > religion > party)")

## 5.4. Religion > Gender > Race > Party  ======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- temp_fxn(v, 7, p_template, type = 4)

p_qoi$results <- "raw data" # no weight

ggdt4 <- rbind(p_qoi) %>%
  mutate(ranking = "Pr(religion > gender > race > party)")



ggdt_unadjusted <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)

save(
  list = c(
    "ggdt_unadjusted"
  ),
  file = here("data", "tidy", "PL_unadjusted.Rda")
)
