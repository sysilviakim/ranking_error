# Setup ========================================================================
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

# Originally applications_regression_ipw.R =====================================

## Get data, get bias-correction weights =======================================
## Now already performed within 03_bias_correction.R
.fix_pid <- 4 # Moderate
.fix_male <- 1 # Male
.fix_age <- 40 # close to the average of 52.76
.fix_edu <- 3 # some college

n_bootstrap <- 1000
set.seed(123)
seed_list <- sample(
  1:max(n_bootstrap * 10, 10000), n_bootstrap,
  replace = FALSE
)
seed_list <- as.list(seed_list)
out_ipw <- list()

# using multiple CPU cores
## (https://stackoverflow.com/questions/70213799/)
plan(multisession)

## UNRELIABLE VALUE: One of the ‘future.apply’ iterations (‘future_sapply-1’)
## unexpectedly generated random numbers without declaring so.
## There is a risk that those random numbers are not statistically sound
## and the overall results might be invalid.
## To fix this, specify 'future.seed=TRUE'. This ensures that proper,
## parallel-safe random numbers are produced via the L'Ecuyer-CMRG method.
## To disable this check, use 'future.seed = NULL', or
## set option 'future.rng.onMisuse' to "ignore".

## Test full reproducibility
## temp_first <- boot_luce_temp(identity_data, first(seed_list))
## temp_first_retry <- boot_luce_temp(identity_data, first(seed_list))
## identical(temp_first, temp_first_retry)
out_ipw <- future_sapply(
  seed_list,
  boot_luce_temp,
  data = identity_data,
  future.seed = TRUE
)
message("Creation of out_ipw complete.")

out_ipw_df <- data.frame(
  ideology = vector("numeric", n_bootstrap),
  mean = vector("numeric", n_bootstrap),
  ranking = vector("character", n_bootstrap)
)

# Store all results in a data frame
out_ipw_df <- seq(n_bootstrap) %>%
  map(
    ~ data.frame(
      ideology = out_ipw[, .x]$ideology,
      mean = out_ipw[, .x]$mean,
      ranking = out_ipw[, .x]$ranking
    )
  ) %>%
  bind_rows()
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

save(PL_ipw, file = here("data", "tidy", "PL_ipw_sapply.Rda"))
message("Creation of PL_ipw complete.")

# Originally applications_regression_raw.R =====================================
gc(reset = TRUE)

## 1. Get data, get bias-correction weights ====================================
## Now already performed within 03_bias_correction.R
## Sanity check
plot(
  dt$weight, dt$w_multiplied,
  xlim = c(0, 6), ylim = c(0, 6)
)
abline(a = 0, b = 1, col = "firebrick4", lty = 2)

## 2. Data processing ==========================================================
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

## 3. Create indexed data ======================================================
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

## 4. Run Rank-order logit model ===============================================
# Estimating parameters (no bias correction)
m <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + male + age + race4 + educ + region,
  mdat, # Data
  reflevel = "gender", # Base category
  weight = weight # Survey weights
)

# Raw result
summary(m)

## 5. Get predicted probabilities ==============================================
### 5.1. Gender > Race > Party > Religion ======================================
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

### 5.2. Party > Gender > Race > Religion ======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 2)
p_qoi$results <- "raw data" # no weight

ggdt2 <- p_qoi %>%
  mutate(ranking = "Pr(party > gender > race > religion)")

### 5.3. Gender > Race > Party > Religion ======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 3)

p_qoi$results <- "raw data" # no weight

ggdt3 <- p_qoi %>%
  mutate(ranking = "Pr(gender > race > religion > party)")

### 5.4. Religion > Gender > Race > Party  =====================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- regress_clarify_temp(v, 7, p_template, type = 4)
p_qoi$results <- "raw data" # no weight

ggdt4 <- p_qoi %>%
  mutate(ranking = "Pr(religion > gender > race > party)")

## 6. Visualize the final results  =============================================
PL_raw <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)

## This will be used in R/Fig9.R
save(PL_raw, file = here("data", "tidy", "PL_raw.Rda"))
message("Creation of PL_raw complete.")

# Create Figure 9 ==============================================================
gc(reset = TRUE)

# Merging Fig9.R, originally 06_applications_regress_visualize.R 
load(here("data", "tidy", "PL_raw.Rda"))
load(here("data", "tidy", "PL_ipw_sapply.Rda"))

gg_raw <- PL_raw %>%
  filter(results == "raw data") %>%
  arrange(ranking)
gg_ipw <- PL_ipw %>% arrange(ranking)

# Recombine the two estimates
ggdt_all <- rbind(gg_raw, gg_ipw)

p <- ggdt_all %>%
  ggplot(aes(x = ideology, y = mean, color = results, shape = results)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_pointrange(
    aes(ymin = low, ymax = up),
    position = position_dodge2(width = 0.5)
  ) +
  scale_color_manual(values = c("darkcyan", alpha("dimgray", 0.3))) +
  facet_wrap(~ranking) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ylim(0, 0.5) +
  labs(
    caption = paste0(
      "Predictions for a 40-year-old white male ",
      "with some college education,\nindependent, and in Northeast"
    )
  )

p +
  theme_bw() + 
  theme(
    plot.caption = element_text(hjust = 0),
    strip.text.x = element_text(angle = 0, hjust = 0),
    strip.background = element_rect(fill = "white"),
    legend.position = c(0.35, 0.9),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = alpha("lightblue", 0),
      size = 0.5, linetype = "solid"
    )
  )

## Figure 9, formerly placketluce_weight_all_weight-boot.pdf
ggsave(
  here("fig", "Fig9.pdf"),
  width = 6, height = 5
)
message("Creation of Fig9 complete.")

## First difference between the most liberal and conservative ==================
## Bias-corrected: 0.31268234  - 0.06695451 = 0.2457278
corrected_diff <- ggdt_all %>%
  filter(ideology == 7 | ideology == 1) %>%
  filter(results == "bias-corrected") %>%
  filter(ranking == "Pr(gender > race > party > religion)") %>%
  {.$mean[1] - .$mean[2]}
corrected_diff

## Raw data: 0.20329978 - 0.04637296 = 0.1569268
raw_diff <- ggdt_all %>%
  filter(ideology == 7 | ideology == 1) %>%
  filter(results == "raw data") %>%
  filter(ranking == "Pr(gender > race > party > religion)") %>%
  {.$mean[1] - .$mean[2]}
raw_diff

corrected_diff / raw_diff

## Average probabilities =======================================================
## Bias-corrected: 0.1753277
corrected_avg <- ggdt_all %>%
  filter(results == "bias-corrected") %>%
  filter(ranking == "Pr(gender > race > religion > party)") %>%
  {mean(.$mean)}
corrected_avg

## Raw data: 0.1088795
raw_avg <- ggdt_all %>%
  filter(results == "raw data") %>%
  filter(ranking == "Pr(gender > race > religion > party)") %>%
  {mean(.$mean)}
raw_avg

corrected_avg / raw_avg
