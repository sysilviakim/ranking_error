source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main
load(here("data", "tidy", "bias_correction.Rda"))

# Experiment by Yuki (7/1)
## Renormalize weights
main_ipw$weights$w_star <- (main_ipw$weights$w / (sum(main_ipw$weights$w))) * 24

## Checking
hist(main_ipw$weights$w_star, breaks = 20)
sum(main_ipw$weights$w_star)

## Recode w_multiplied
dt <- dt %>%
  left_join(main_ipw$weights, by = "ranking") %>%
  mutate(w_multiplied_old = w_multiplied,
         w_multiplied = weight * w_star)

## Check --- 
plot(dt$w_multiplied, dt$w_multiplied_old,
     xlim = c(0, 8), ylim = c(0, 8))
abline(a = 0, b = 1, lty = 2)  

# Bias correction ==============================================================
# Direct Bias Correction
avg_rank.w <- main_direct$qoi %>%
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
avg_rank.i <- lm_robust(party ~ 1, dt, weights = w_multiplied) %>% tidy()
avg_rank.i <- rbind(
  avg_rank.i, lm_robust(religion ~ 1, dt, weights = w_multiplied) %>% tidy()
)
avg_rank.i <- rbind(
  avg_rank.i, lm_robust(gender ~ 1, dt, weights = w_multiplied) %>% tidy()
)
avg_rank.i <- rbind(
  avg_rank.i,
  lm_robust(race_ethnicity ~ 1, dt, weights = w_multiplied) %>% tidy()
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
      x = conf.high + 0.15,
      label = round(estimate, 1.5)
    ),
    position = position_dodge(width = width_par),
    size = 2,
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
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    text = element_text(size = 8)
  )
ggsave(
  here("fig", "weight-avg-rank-sample.pdf"),
  width = 4.5, height = 3
)

# Quantities for comparison ====================================================
avg_gg_comb %>%
  arrange(dt, estimate) %>%
  group_by(dt) 

# Party v. religion
(3.27 - 2.60) / (3.00 - 2.56) # Direct/Unadjusted
(3.22 - 2.61) / (3.00 - 2.56) # IPW/Unadjusted

# Party v. gender
(3.28 - 1.65) / (3.00 - 1.95) # Direct/Unadjusted
(3.22 - 1.72) / (3.00 - 1.95) # IPW/Unadjusted
