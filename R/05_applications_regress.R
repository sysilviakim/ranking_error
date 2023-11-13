source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Grab rankings and weights
imp_w <- read_csv(here::here("data/tidy", "temp_weight.csv")) %>%
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
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)

# Downsize data
dt <- main %>%
  mutate(
    id = 1:nrow(main),
    black = ifelse(race == 2, 1, 0)
  ) %>%
  rename(
    ch.party = app_identity_1,
    ch.religion = app_identity_2,
    ch.gender = app_identity_3,
    ch.race = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  select(starts_with("ch"), id, ideo7, pid7, race, bias_weight) %>%
  drop_na()


head(dt) # check
hist(dt$bias_weight, border = "white") # good, no extremely small weight
abline(v = 1, lty = 2, lwd = 2, col = "darkred")


# Run Rank-order logit =========================================================
library(mlogit)

# Transform into mlogit format
dt <- as.data.frame(dt)
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


# Estimating parameters (no weight)
m <- mlogit(
  ch ~ 1 | ideo7, # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender" # Base category
)


# Raw result
summary(m)

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
  
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i)
  e_XB_gen <- 1

  # Here, we want to compute the probability for one unique ranking
  # Prob (party, race, religion, gender)
  # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
  # This is multiplication of three multinomial choices
  p <- e_XB_party / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_race / (e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_reli / (e_XB_reli + e_XB_gen) * 
    e_XB_gen / e_XB_gen # prob(choosing gender out of gender)

   # we want to generate 24 ps. They should sum up to one.
  
  p_qoi[i, 2] <- mean(p)
  p_qoi[i, 3] <- quantile(p, prob = 0.025) # if we bootstrap the whole thing, we don't need to save this
  p_qoi[i, 4] <- quantile(p, prob = 0.975) # if we bootstrap the whole thing, we don't need to save this
}

p_qoi




# Estimating parameters (with weight)
m2 <- mlogit(
  ch ~ 1 | ideo7, # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender", # Base category
  weight = bias_weight
)


# Raw result
summary(m2)


saveRDS(m, file="m1.RData") # save mlogit object for checking
saveRDS(m2, file="m2.RData") # save mlogit object for checking


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
  e_XB_party <- exp(v$`(Intercept):party` + v$`ideo7:party` * i)
  e_XB_race <- exp(v$`(Intercept):race` + v$`ideo7:race` * i)
  e_XB_reli <- exp(v$`(Intercept):religion` + v$`ideo7:religion` * i)
  e_XB_gender <- 1
  
  # Prob: party, race, religion, gender
  # Prob(party) * Prob(race) * Prob(religion)
  p <- e_XB_party / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_race / (e_XB_race + e_XB_reli + e_XB_gen) *
    e_XB_reli / (e_XB_reli + e_XB_gen) * 
    e_XB_gen / e_XB_gen # prob(choosing gender out of gender)
  

  p_qoi2[i, 2] <- mean(p)
  p_qoi2[i, 3] <- quantile(p, prob = 0.025)
  p_qoi2[i, 4] <- quantile(p, prob = 0.975)
}



p_qoi$results <- "raw data" # no weight
p_qoi2$results <- "with bias correction" # with weight


ggdt <- rbind(p_qoi, p_qoi2)

ggdt %>%
  ggplot(aes(x = ideology, y = mean, color = results)) +
  geom_point() +
  geom_pointrange(aes(ymin = low, ymax = up)) +
  scale_color_manual(values = c("darkcyan", "darkred")) +
  #  facet_wrap(~ type) +
  theme_bw() +
  xlim(1, 7) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ggtitle("Ranking 1423 (Party, Race, Religion, Gender)") -> p

p

ggsave(here::here("fig", "placketluce_weight.pdf"),
  width = 5, height = 3.5
)



# Below, Yuki is exploring how to modify Clarify to accomodate PL
# Predictions of Multinomial Choice Probabilities
m3 <- mlogit(
  ch ~ 1 | ideo7, # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender", # Base category
  weight = bias_weight
)



# investigating why clarify does not support mlogit
## https://github.com/IQSS/clarify/blob/main/R/sim_setx.R
summary(m3)
set.seed(123)
sim <- sim(m3)

#party <- sim_setx(sim_coefs, 
#                  x = list(ideo7 = 4),
#                     outcome = "party")
# won't work


check_sim_apply_wrapper_ready(sim) # works
is_misim <- inherits(sim, "clarify_misim") # works
dat <- {
  if (is_misim)
    do.call("rbind", lapply(sim$fit, insight::get_predictors, verbose = FALSE))
  else
    insight::get_predictors(sim$fit, verbose = FALSE)
} # works

x = list(ideo7 = 4) # works
newdata <- process_x(x, dat, "x") # NOT working, I can't find process_x() anywhere


args <- list(x, newdata = newdata, vcov = FALSE, type = NULL)

p <- try(do.call(marginaleffects::get_predict, args))
(length(p) == 0L || is_error(p))
# I know that this part is stopping the function
    


# Yuki commented out below, 11/12/2023
# # Additional checks with mean ranks (recreating Figure 7)
# 
# dt2 <- dt %>%
#   rename(
#     party = ch.party,
#     religion = ch.religion,
#     gender = ch.gender,
#     race = ch.race
#   )
# 
# ## no weight
# mrank <- lm_robust(party ~ 1, dt2) %>% tidy()
# mrank <- rbind(mrank, lm_robust(religion ~ 1, dt2) %>% tidy())
# mrank <- rbind(mrank, lm_robust(gender ~ 1, dt2) %>% tidy())
# mrank <- rbind(mrank, lm_robust(race ~ 1, dt2) %>% tidy())
# 
# 
# ## with weight
# mrank2 <- lm_robust(party ~ 1, dt2,
#   weight = bias_weight
# ) %>% tidy()
# mrank2 <- rbind(mrank2, lm_robust(religion ~ 1, dt2,
#   weight = bias_weight
# ) %>% tidy())
# mrank2 <- rbind(mrank2, lm_robust(gender ~ 1, dt2,
#   weight = bias_weight
# ) %>% tidy())
# mrank2 <- rbind(mrank2, lm_robust(race ~ 1, dt2,
#   weight = bias_weight
# ) %>% tidy())
# 
# mrank$Type <- "raw data"
# mrank2$Type <- "de-biased"
# 
# ggdt <- rbind(mrank, mrank2) %>%
#   rename(
#     low = conf.low,
#     up = conf.high,
#     est = estimate
#   )
# ggdt$outcome <- factor(ggdt$outcome,
#   levels = c("gender", "religion", "race", "party")
# )
# J <- 4
# color_list <- c("#b0015a", "#999999")
# 
# 
# ggplot(ggdt, aes(
#   x = fct_rev(outcome),
#   y = est, color = Type
# )) +
#   geom_point(
#     aes(shape = Type),
#     size = 2,
#     position = position_dodge(width = 0.5)
#   ) +
#   # Reorder by point estimate
#   geom_linerange(
#     aes(ymin = low, ymax = up),
#     lwd = 1, position = position_dodge(width = 0.5)
#   ) +
#   scale_color_manual(values = color_list) +
#   theme_bw() +
#   coord_flip() +
#   xlab("") +
#   ylab("") +
#   scale_y_continuous(limits = c(1, J), breaks = seq(J)) +
#   geom_hline(yintercept = (J + 1) / 2, linetype = "dashed") +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"),
#     legend.spacing.x = unit(0, "cm"),
#     legend.spacing.y = unit(0, "cm"),
#     plot.title = element_text(face = "bold")
#   ) +
#   ggtitle("Recreation of Figure 7 via Weighting")
# 
# ggsave(here::here("fig", "weighting_check.pdf"),
#   width = 6.5, height = 4
# )
# 
# 
# # Try Placket-Luce Package does not allow covariates
# library(PlackettLuce)
# out <- PlackettLuce(dt[, 1:4])
# coef(out, log = FALSE)
