source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

# Data processing ==============================================================
main <- main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

# Reference set: (party, religion, gender, race)
data <- main %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity, weight
  )

# Direct bias correction
d <- imprr_direct(
  data = data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data$weight,
  n_bootstrap = 1000
)

# Inverse probability weighting
w <- imprr_weights(
  data = data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1000
)

print(d)
print(w)

# Downsize data
dt <- main %>%
  mutate(
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race_ethnicity = app_identity_4,
    ranking = app_identity
  ) %>%
  left_join(w$weights, by = "ranking") %>%
  mutate(dual_weight = weight * w) %>%
  select(
    party, religion, gender, race_ethnicity, race, ranking, weight, dual_weight
  )

# Bias correction ==============================================================
# Direct Bias Correction
avg_rank.w <- d$qoi %>%
  filter(qoi == "average rank") %>%
  ungroup(qoi) %>%
  mutate(
    item = case_when(
      item == "app_identity_1" ~ "party",
      item == "app_identity_2" ~ "religion",
      item == "app_identity_3" ~ "gender",
      item == "app_identity_4" ~ "race_ethnicity"
    ),
    estimate = mean,
    conf.low = lower,
    conf.high = upper,
    dt = "Direct"
  ) %>%
  select(item, estimate, conf.low, conf.high, dt)

# IPW
avg_rank.i <- as.data.frame(NA)
avg_rank.i <- lm_robust(party ~ 1, dt, weights = dual_weight) %>% tidy()
avg_rank.i <- rbind(
  avg_rank.i, lm_robust(religion ~ 1, dt, weights = dual_weight) %>% tidy()
)
avg_rank.i <- rbind(
  avg_rank.i, lm_robust(gender ~ 1, dt, weights = dual_weight) %>% tidy()
)
avg_rank.i <- rbind(
  avg_rank.i,
  lm_robust(race_ethnicity ~ 1, dt, weights = dual_weight) %>% tidy()
)
avg_rank.i$dt <- "IPW"
avg_rank.i <- avg_rank.i %>%
  rename(item = outcome) %>%
  select(item, estimate, conf.low, conf.high, dt)

# Raw Data
avg_rank <- as.data.frame(NA)
avg_rank <- lm_robust(party ~ 1, dt, weights = weight) %>% tidy()
avg_rank <- rbind(
  avg_rank, lm_robust(religion ~ 1, dt, weights = weight) %>% tidy()
)
avg_rank <- rbind(
  avg_rank, lm_robust(gender ~ 1, dt, weights = weight) %>% tidy()
)
avg_rank <- rbind(
  avg_rank, lm_robust(race_ethnicity ~ 1, dt, weights = weight) %>% tidy()
)
avg_rank$dt <- "Raw Data"
avg_rank <- avg_rank %>%
  rename(item = outcome) %>%
  select(item, estimate, conf.low, conf.high, dt)

# Visualization ================================================================
avg_gg_comb <- rbind(
  avg_rank,
  avg_rank.w,
  avg_rank.i
)
width_par <- 0.5

p <- avg_gg_comb %>%
  mutate(
    item = case_when(
      item == "party" ~ "Party",
      item == "religion" ~ "Religion",
      item == "race_ethnicity" ~ "Race/ethnicity",
      item == "gender" ~ "Gender"
    )
  ) %>%
  ggplot(
    aes(
      y = fct_reorder(item, -estimate, mean),
      x = estimate, group = dt, color = dt
    )
  ) +
  scale_fill_brewer(palette = "Accent") +
  geom_vline(
    xintercept = 2.5, lty = 2, color = alpha("black", 0.5), linewidth = 0.3
  ) +
  geom_point(
    aes(shape = dt),
    position = position_dodge(width = width_par), size = 1.5
  ) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
    width = 0,
    position = position_dodge(width_par), size = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Direct" = "darkcyan",
      "IPW" = "maroon4",
      "Raw Data" = alpha("dimgray", 0.5)
    )
  ) +
  geom_text(
    aes(
      x = conf.high + 0.1,
      label = round(estimate, 1.5)
    ),
    position = position_dodge(width = width_par),
    size = 2.5,
    color = "black",
    family = "CM Roman"
  ) +
  xlim(1, 4.1) +
  ylab("") +
  xlab("") +
  theme_classic(base_rect_size = 11 / 44)

pdf_default(p) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    text = element_text(size = 10)
  )
ggsave(
  here("fig", "weight-avg-rank-sample.pdf"),
  width = 4.5, height = 3
)

# Quantities for comparison ====================================================
avg_gg_comb %>%
  arrange(dt, estimate) %>%
  group_by(dt) %>%
  mutate(diff = estimate - lag(estimate))

# # Gender v. race
# 0.836/0.543  # Direct/Unadjusted
# 0.720/0.543  # IPW/Unadjusted

# Party v. religion
0.680 / 0.437 # Direct/Unadjusted
0.617 / 0.437 # IPW/Unadjusted

# Party v. gender
(3.28 - 1.65) / (3.00 - 1.95) # Direct/Unajusted
(3.22 - 1.72) / (3.00 - 1.95)
