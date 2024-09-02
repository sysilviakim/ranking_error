source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
load(here("data", "tidy", "bias_correction.Rda"))

# Subset and wrangle data that we need =========================================
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

## Reference set: (party, religion, gender, race)
identity_data <- main %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity, anc_correct_id_alphabet, anc_correct_id_exact,
    weight, partisan
  )

# Stratification by bootstrap  =================================================
out_stratification <- stratified_avg(
  data = identity_data,
  var_stratum = "partisan",
  seed = 1245,
  J = 4,
  n_bootstrap = 1000,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = "weight",
  labels = c("party", "religion", "gender", "race_ethnicity")
)

avg_rank.s <- out_stratification %>%
  group_by(item) %>%
  summarize(
    estimate = mean(mean),
    conf.low = quantile(mean, prob = 0.025),
    conf.high = quantile(mean, prob = 0.975)
  ) %>%
  mutate(dt = "Assumption 5")

# Methods based on Assumptions 3-4 =============================================
## Assumption 3 --- theta = observed data estimates
# Raw Data
avg_rank <- as.data.frame(NA)
avg_rank <- 
  lm_robust(app_identity_1 ~ 1, identity_data, weights = weight) %>% tidy()
avg_rank <- rbind(
  avg_rank, 
  lm_robust(app_identity_2 ~ 1, identity_data, weights = weight) %>% tidy()
)
avg_rank <- rbind(
  avg_rank, 
  lm_robust(app_identity_3 ~ 1, identity_data, weights = weight) %>% tidy()
)
avg_rank <- rbind(
  avg_rank, 
  lm_robust(app_identity_4 ~ 1, identity_data, weights = weight) %>% tidy()
)
avg_rank$dt <- "Assumption 3"
avg_rank <- avg_rank %>%
  rename(item = outcome) %>%
  mutate(
    item = case_when(
      item == "app_identity_1" ~ "party",
      item == "app_identity_2" ~ "religion",
      item == "app_identity_3" ~ "gender",
      item == "app_identity_4" ~ "race_ethnicity"
    )
  ) %>%
  select(item, estimate, conf.low, conf.high, dt)

## Assumption 4 --- theta = theta-z
avg_rank.d <- main_direct$qoi %>%
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
    dt = "Assumption 4"
  ) %>%
  select(item, estimate, conf.low, conf.high, dt)

# Visualization ================================================================
avg_gg_comb <- rbind(
  avg_rank.s,
  avg_rank,
  avg_rank.d
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
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0,
    position = position_dodge(width_par), 
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Assumption 5" = "darkcyan",
      "Assumption 4" = "maroon4",
      "Assumption 3" = "dimgray"
    )
  ) +
  geom_text(
    aes(
      x = conf.high + 0.15,
      label = formatC(estimate, digits = 2, format = "f")
    ),
    position = position_dodge(width = width_par),
    size = 2,
    color = "black"
  ) +
  xlim(1, 4.1) +
  ylab("") +
  xlab("") +
  theme_classic(base_rect_size = 11 / 44)

p +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    text = element_text(size = 8)
  )

## Figure 10, formerly weight-avg-rank-theta-partisan-strength.pdf
ggsave(
  here("fig", "AtsusakaKimFig10.pdf"),
  width = 4.5, height = 3
)
