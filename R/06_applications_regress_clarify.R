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
      ## Gender > Race > Party > Religion
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
    race = as.factor(race),
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

# Estimating parameters (with weight)
m2 <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + male + age + race4 + educ + region,
  mdat, # Data
  reflevel = "gender", # Base category
  weight = w_multiplied # Survey weights X bias-correction weights
)

# Raw result
summary(m)
summary(m2)

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
.fix_edu <- 4 # from 1 to 6

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

# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi2 <- temp_fxn(v, 7, p_template, type = 1)

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight

ggdt1 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(gender > race > party > religion)")

## 5.2. Party > Gender > Race > Religion =======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- temp_fxn(v, 7, p_template, type = 2)

# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi2 <- temp_fxn(v, 7, p_template, type = 2)

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight

ggdt2 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(party > gender > race > religion)")

## 5.3. Gender > Race > Party > Religion =======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- temp_fxn(v, 7, p_template, type = 3)

# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi2 <- temp_fxn(v, 7, p_template, type = 3)

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight

ggdt3 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(gender > race > religion > party)")

## 5.4. Religion > Gender > Race > Party  ======================================
# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi <- temp_fxn(v, 7, p_template, type = 4)

# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)
v <- sim_coefs$sim.coefs %>% as_tibble()
p_qoi2 <- temp_fxn(v, 7, p_template, type = 4)

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight

ggdt4 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(religion > gender > race > party)")

# 6. Visualize the final results  ==============================================
ggdt_all <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)
p <- ggdt_all %>%
  ggplot(aes(x = ideology, y = mean, color = results, shape = results)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_pointrange(aes(ymin = low, ymax = up),
    position = position_dodge2(width = 0.5)
  ) +
  scale_color_manual(values = c("darkcyan", alpha("dimgray", 0.3))) +
  facet_wrap(~ranking) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  labs(
    caption = paste0(
      "Predictions for 40yo, white, male, ",
      "with mid-level edu, independent, in Northeast"
    )
  )

pdf_default(p) +
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
ggsave(
  here("fig", "placketluce_weight_all_weight.pdf"),
  width = 6, height = 5
)

# First difference between the most liberal and conservative
# ggdt1
# Bias-corrected
# > 0.29862563 - 0.06342958
# [1] 0.235196

# Raw data
# > 0.19450703 - 0.04083227
# [1] 0.1536748

# > 0.235196/0.1536748
# [1] 1.530479

ggdt3 %>%
  group_by(results) %>%
  summarize(mean(mean))
