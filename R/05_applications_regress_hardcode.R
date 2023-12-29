source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# devtools::install_github("sysilviakim/ranking", force = T)
library(ranking)

# Grab rankings and weights
imp_w <- read_csv(here("data/tidy", "temp_weight.csv")) %>%
  mutate(
    app_identity = as.character(ranking),
    bias_weight = weight
  ) %>%
  dplyr::select(app_identity, bias_weight)

imp_w
hist(imp_w$bias_weight, breaks = 20) # 6 rankings should not exist, thus  w = 0

# Grab main data
main <- df_list$main

# Data processing ==============================================================
# Fix some weird situation ---> haven::labelled to numeric
main <- main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

# Reference set: (party, religion, gender, race)
# Downsize data
dt <- main %>%
  mutate(
    id = 1:nrow(main),
    race = as.factor(race),
    region = as.factor(region),
    male = ifelse(gender3==1, 1, 0) # male binary variable
  ) %>%
  rename(
    ch.party = app_identity_1,
    ch.religion = app_identity_2,
    ch.gender = app_identity_3,
    ch.race = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  select(
    starts_with("ch"), id,
    ideo7, pid7, educ, race,
    age, partisan, region, male,
    bias_weight
  ) %>%
  drop_na() %>%
  ## Aggregate Native American, Other, and Middle Eastern into a single category
  ## for race (otherwise, singularity issue)
  mutate(
    race6 = case_when(
      as.numeric(race) %in% c(5, 7, 8) ~ 5,
      TRUE ~ as.numeric(race)
    ),
    race6 = as.factor(race6)
  )

head(dt) # check
hist(dt$bias_weight, border = "white") # good, no extremely small weight
abline(v = 1, lty = 2, lwd = 2, col = "darkred")

# Create indexed data ==========================================================
library(mlogit)

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

# Run Rank-order logit model ===================================================
# Estimating parameters (no weight)
m <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + male + age + race6 + educ + region,
  # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender" # Base category
)

# Estimating parameters (with weight)
m2 <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + male + age + race6 + educ + region,
  # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender", # Base category
  weight = bias_weight
)

# Raw result
summary(m)
summary(m2)

###########################################################
# Predicted Prob for Gender > Race > Party > Religion
###########################################################

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
library(clarify)
set.seed(123)
sim_coefs <- sim(m)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)

for (i in 1:7) {
  
  
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                    v$`pid7:party` * .fix_pid +
                    v$`male:party` * .fix_male +
                    v$`age:party` *  .fix_age +
                    v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gen <- 1
  
  # Here, we want to compute the probability for one unique ranking
  # Prob (party, race, religion, gender)
  # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
  # This is multiplication of three multinomial choices
  p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
    e_XB_party / (e_XB_reli + e_XB_party) * 
    e_XB_reli / e_XB_reli # prob(choosing gender out of gender)
  
  # we want to generate 24 ps. They should sum up to one.
  
  p_qoi[i, 2] <- mean(p)
  p_qoi[i, 3] <- quantile(p, prob = 0.025) # if we bootstrap the whole thing, we don't need to save this
  p_qoi[i, 4] <- quantile(p, prob = 0.975) # if we bootstrap the whole thing, we don't need to save this
}

p_qoi



# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi2 <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)
for (i in 1:7) {
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Prob: party, race, religion, gender
  # Prob(party) * Prob(race) * Prob(religion)
  p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
    e_XB_party / (e_XB_reli + e_XB_party) * 
    e_XB_reli / e_XB_reli 
  
  p_qoi2[i, 2] <- mean(p)
  p_qoi2[i, 3] <- quantile(p, prob = 0.025)
  p_qoi2[i, 4] <- quantile(p, prob = 0.975)
}

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight


ggdt1 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(gender > race > party > religion)")


###########################################################
# Predicted Prob for Party > Gender > Race > Religion
###########################################################


# Generate 1000 sets of parameters (parametric bootstrap)
library(clarify)
set.seed(123)
sim_coefs <- sim(m)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)

for (i in 1:7) {
  
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Here, we want to compute the probability for one unique ranking
  # Prob (party, race, religion, gender)
  # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
  # This is multiplication of three multinomial choices
  
  p <- e_XB_party / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_gen / (e_XB_race + e_XB_gen + e_XB_reli) *
    e_XB_race / (e_XB_race + e_XB_reli) * 
    e_XB_reli / e_XB_reli 
  
  # we want to generate 24 ps. They should sum up to one.
  
  p_qoi[i, 2] <- mean(p)
  p_qoi[i, 3] <- quantile(p, prob = 0.025) # if we bootstrap the whole thing, we don't need to save this
  p_qoi[i, 4] <- quantile(p, prob = 0.975) # if we bootstrap the whole thing, we don't need to save this
}

p_qoi


# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi2 <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)
for (i in 1:7) {
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Prob: party, gender, race, religion
  # Prob(party) * Prob(race) * Prob(religion)
  p <- e_XB_party / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_gen / (e_XB_race + e_XB_gen + e_XB_reli) *
    e_XB_race / (e_XB_race + e_XB_reli) * 
    e_XB_reli / e_XB_reli 
  
  p_qoi2[i, 2] <- mean(p)
  p_qoi2[i, 3] <- quantile(p, prob = 0.025)
  p_qoi2[i, 4] <- quantile(p, prob = 0.975)
}

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight


ggdt2 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(party > gender > race > religion)")

###########################################################
# Predicted Prob for Gender > Race > Party > Religion  
###########################################################


# Generate 1000 sets of parameters (parametric bootstrap)
library(clarify)
set.seed(123)
sim_coefs <- sim(m)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)

for (i in 1:7) {
  
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Here, we want to compute the probability for one unique ranking
  # Prob (party, race, religion, gender)
  # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
  # This is multiplication of three multinomial choices
  
  p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
    e_XB_reli / (e_XB_reli + e_XB_party) * 
    e_XB_party / e_XB_party 
  
  # we want to generate 24 ps. They should sum up to one.
  
  p_qoi[i, 2] <- mean(p)
  p_qoi[i, 3] <- quantile(p, prob = 0.025) # if we bootstrap the whole thing, we don't need to save this
  p_qoi[i, 4] <- quantile(p, prob = 0.975) # if we bootstrap the whole thing, we don't need to save this
}

p_qoi


# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi2 <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)
for (i in 1:7) {
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Prob: party, gender, race, religion
  # Prob(party) * Prob(race) * Prob(religion)
  p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
    e_XB_reli / (e_XB_reli + e_XB_party) * 
    e_XB_party / e_XB_party 
  
  p_qoi2[i, 2] <- mean(p)
  p_qoi2[i, 3] <- quantile(p, prob = 0.025)
  p_qoi2[i, 4] <- quantile(p, prob = 0.975)
}

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight


ggdt3 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(gender > race > religion > party)")


###########################################################
# Predicted Prob for Religion > Gender > Race > Party  
###########################################################

# Generate 1000 sets of parameters (parametric bootstrap)
library(clarify)
set.seed(123)
sim_coefs <- sim(m)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)

for (i in 1:7) {
  
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Here, we want to compute the probability for one unique ranking
  # Prob (party, race, religion, gender)
  # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
  # This is multiplication of three multinomial choices
  
  p <- e_XB_reli / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_gen / (e_XB_race + e_XB_gen + e_XB_party) *
    e_XB_race / (e_XB_race + e_XB_party) * 
    e_XB_party / e_XB_party 
  
  # we want to generate 24 ps. They should sum up to one.
  
  p_qoi[i, 2] <- mean(p)
  p_qoi[i, 3] <- quantile(p, prob = 0.025) # if we bootstrap the whole thing, we don't need to save this
  p_qoi[i, 4] <- quantile(p, prob = 0.975) # if we bootstrap the whole thing, we don't need to save this
}

p_qoi


# Generate 1000 sets of parameters (parametric bootstrap)
set.seed(123)
sim_coefs <- sim(m2)

v <- sim_coefs$sim.coefs %>% as_tibble()

p_qoi2 <- data.frame(
  ideology = 1:7,
  mean = NA,
  low = NA,
  up = NA
)
for (i in 1:7) {
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i +
                      v$`pid7:party` * .fix_pid +
                      v$`male:party` * .fix_male +
                      v$`age:party` *  .fix_age +
                      v$`educ:party` * .fix_edu)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i +
                     v$`pid7:race` * .fix_pid +
                     v$`male:race` * .fix_male +
                     v$`age:race` *  .fix_age +
                     v$`educ:race` * .fix_edu)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i +
                     v$`pid7:religion` * .fix_pid +
                     v$`male:religion` * .fix_male +
                     v$`age:religion` *  .fix_age +
                     v$`educ:religion` * .fix_edu)
  e_XB_gender <- 1
  
  # Prob: party, gender, race, religion
  # Prob(party) * Prob(race) * Prob(religion)
  p <- e_XB_reli / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_gen / (e_XB_race + e_XB_gen + e_XB_party) *
    e_XB_race / (e_XB_race + e_XB_party) * 
    e_XB_party / e_XB_party 
  
  p_qoi2[i, 2] <- mean(p)
  p_qoi2[i, 3] <- quantile(p, prob = 0.025)
  p_qoi2[i, 4] <- quantile(p, prob = 0.975)
}

p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "our methods" # with weight


ggdt4 <- rbind(p_qoi, p_qoi2) %>%
  mutate(ranking = "Pr(religion > gender > race > party)")



ggdt_all <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)


ggdt_all %>%
  ggplot(aes(x = ideology, y = mean, color = results)) +
  geom_point(position = position_dodge2(width = 0.4)) +
  geom_pointrange(aes(ymin = low, ymax = up),
                  position = position_dodge2(width = 0.4)) +
  scale_color_manual(values = c("darkcyan", "darkred")) +
  facet_wrap(~ ranking, scales = "free") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  labs(caption = "Predictions for 40yo, white, male, with mid-level edu, independent, in northeast") +
  theme(plot.caption = element_text(hjust = 0),
        strip.text.x = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = "white"),
        legend.position = c(0.35, 0.9),
        legend.title = element_blank(),
        legend.background = element_rect(fill=alpha("lightblue",0), 
                                         size=0.5, linetype="solid"))  -> p

p

ggsave(here::here("fig", "placketluce_weight_all.pdf"),
       width = 6, height = 5
)

