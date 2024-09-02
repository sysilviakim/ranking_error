source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))
load(here("data", "tidy", "bias_correction.Rda"))

# Data processing + frequencies ================================================
# Reference set: (party, religion, gender, race)
# code attention check here
data <- main %>%
  mutate(
    attention_berinsky = ifelse(berinsky_fail == 1, 0, 1),
    attention_ternovski = ifelse(ternovski_fail == 1, 0, 1)
  ) %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_correct_identity, anc_correct_id_alphabet, anc_correct_id_exact,
    app_identity_repeat, app_identity,
    attention_berinsky, attention_ternovski, weight,
    race4, gender3, pid3, educ4
  )

# Everyone receives two attention checks
table(main$berinsky_fail, useNA = "always")
table(main$ternovski_fail, useNA = "always")

# Half receives Alphabet, the other half Exact
table(main$anc_correct_id_alphabet, useNA = "always")
table(main$anc_correct_id_exact, useNA = "always")
table(
  main$anc_correct_id_alphabet, main$anc_correct_id_exact,
  useNA = "always"
)

table(main$app_identity_repeat, useNA = "always")

dt_alpha <- data %>% filter(!is.na(anc_correct_id_alphabet))
dt_exact <- data %>% filter(!is.na(anc_correct_id_exact))
dt_repeat <- data %>%
  filter(!is.na(app_identity_repeat)) %>%
  mutate(repeat_correct = ifelse(app_identity_repeat == app_identity, 1, 0))

dt_checkI <- data %>% filter(!is.na(attention_ternovski))
dt_checkII <- data %>% filter(!is.na(attention_berinsky))

table(dt_repeat$repeat_correct)

# Estimate the proportion of random responses (attention/repeated) =============
est_p_random_checks <- rbind(
  lm_robust(repeat_identity ~ 1, main),
  lm_robust(ternovski_fail ~ 1, main),
  lm_robust(berinsky_fail ~ 1, main)
) %>%
  as.data.frame() %>%
  mutate(
    mean = as.numeric(coefficients),
    lower = as.numeric(conf.low),
    upper = as.numeric(conf.high)
  ) %>%
  select(mean, lower, upper)

# Combine the estimated proportions
main_direct$est_p_random
alphabet_direct$est_p_random
exact_direct$est_p_random

d_com <- rbind(
  main_direct$est_p_random,
  alphabet_direct$est_p_random,
  exact_direct$est_p_random,
  est_p_random_checks
)

# Plot the estimated proportion of random responses ============================
d_com$group <- c(
  "Anchor main",
  "Anchor alphabet",
  "Anchor exact",
  "Repeated",
  "Attention I",
  "Attention II"
)

width_par <- 0.8
a <- ggplot(d_com, aes(x = mean, y = group)) +
  geom_point(aes(shape = group, color = group)) +
  geom_errorbar(
    aes(
      xmin = lower,
      xmax = upper,
      color = group
    ),
    width = 0,
    position = position_dodge(width_par),
    linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Anchor main" = "darkcyan",
      "Anchor alphabet" = alpha("darkcyan", 0.5),
      "Anchor exact" = alpha("darkcyan", 0.5),
      "Repeat" = alpha("dimgray", 0.5),
      "Attention I" = alpha("maroon", 0.5),
      "Attention II" = alpha("maroon", 0.5)
    )
  ) +
  geom_text(
    aes(
      x = upper + 0.04,
      label = round(mean, 1.5)
    ),
    position = position_dodge(width = width_par),
    size = 2.5,
    color = "black",
    family = "CM Roman"
  ) +
  xlab("Estimated proportion of random responses") +
  ylab("") +
  theme(legend.position = "null")

# Plot the estimated average ranks =============================================
dt_main <- main_direct$qoi %>%
  filter(qoi == "average rank") %>%
  mutate(dt = "Anchor main")
dt_alpha <- alphabet_direct$qoi %>%
  filter(qoi == "average rank") %>%
  mutate(dt = "Anchor alphabet")
dt_exact <- exact_direct$qoi %>%
  filter(qoi == "average rank") %>%
  mutate(dt = "Anchor exact")

avg_gg_comb <- rbind(
  dt_main,
  dt_alpha,
  dt_exact
) %>%
  mutate(
    item = case_when(
      item == "app_identity_1" ~ "party",
      item == "app_identity_2" ~ "religion",
      item == "app_identity_3" ~ "gender",
      item == "app_identity_4" ~ "race_ethnicity"
    )
  ) %>%
  rename(
    estimate = mean,
    conf.low = lower,
    conf.high = upper
  ) %>%
  ungroup() %>%
  select(item, estimate, conf.low, conf.high, dt)

# Listwise deletion estimates ==================================================
### Create "clean" data
dt_checkI_clean <- dt_checkI %>% filter(attention_ternovski == 1)
dt_checkII_clean <- dt_checkI %>% filter(attention_berinsky == 1)
dt_repeat_clean <- dt_repeat %>% filter(repeat_correct == 1)

est_checkI <- rbind(
  lm_robust(app_identity_1 ~ 1, dt_checkI_clean) %>% tidy(),
  lm_robust(app_identity_2 ~ 1, dt_checkI_clean) %>% tidy(),
  lm_robust(app_identity_3 ~ 1, dt_checkI_clean) %>% tidy(),
  lm_robust(app_identity_4 ~ 1, dt_checkI_clean) %>% tidy()
) %>%
  mutate(dt = "Attention I")

est_checkII <- rbind(
  lm_robust(app_identity_1 ~ 1, dt_checkII_clean) %>% tidy(),
  lm_robust(app_identity_2 ~ 1, dt_checkII_clean) %>% tidy(),
  lm_robust(app_identity_3 ~ 1, dt_checkII_clean) %>% tidy(),
  lm_robust(app_identity_4 ~ 1, dt_checkII_clean) %>% tidy()
) %>%
  mutate(dt = "Attention II")

est_repeat <- rbind(
  lm_robust(app_identity_1 ~ 1, dt_repeat_clean) %>% tidy(),
  lm_robust(app_identity_2 ~ 1, dt_repeat_clean) %>% tidy(),
  lm_robust(app_identity_3 ~ 1, dt_repeat_clean) %>% tidy(),
  lm_robust(app_identity_4 ~ 1, dt_repeat_clean) %>% tidy()
) %>%
  mutate(dt = "Repeat")

est_unadjusted <- rbind(
  lm_robust(app_identity_1 ~ 1, main) %>% tidy(),
  lm_robust(app_identity_2 ~ 1, main) %>% tidy(),
  lm_robust(app_identity_3 ~ 1, main) %>% tidy(),
  lm_robust(app_identity_4 ~ 1, main) %>% tidy()
) %>%
  mutate(dt = "Unadjusted")

est_com <- rbind(
  est_checkI,
  est_checkII,
  est_repeat
) %>%
  mutate(
    item = case_when(
      outcome == "app_identity_1" ~ "party",
      outcome == "app_identity_2" ~ "religion",
      outcome == "app_identity_3" ~ "gender",
      outcome == "app_identity_4" ~ "race_ethnicity"
    )
  ) %>%
  select(item, estimate, conf.low, conf.high, dt)

avg_gg_comb2 <- rbind(avg_gg_comb, est_com)

# Final visualization ==========================================================
b <- avg_gg_comb2 %>%
  mutate(
    item = case_when(
      item == "gender" ~ "Gender",
      item == "race_ethnicity" ~ "Race/ethnicity",
      item == "religion" ~ "Religion",
      item == "party" ~ "Party"
    )
  ) %>%
  ggplot(
    aes(
      y = fct_reorder(item, -estimate, mean),
      x = estimate, group = dt, color = dt
    )
  ) +
  scale_fill_brewer(palette = "Accent") +
  geom_point(
    aes(shape = dt),
    position = position_dodge(width = width_par), size = 1.5
  ) +
  geom_rect(
    aes(
      xmin = est_unadjusted$conf.low[1],
      xmax = est_unadjusted$conf.high[1],
      ymin = 1.45,
      ymax = 0.55
    ),
    color = alpha("gray50", 0),
    fill = alpha("gray50", 0.01)
  ) +
  geom_rect(
    aes(
      xmin = est_unadjusted$conf.low[2],
      xmax = est_unadjusted$conf.high[2],
      ymin = 2.45,
      ymax = 1.55
    ),
    color = alpha("gray50", 0),
    fill = alpha("gray50", 0.01)
  ) +
  geom_rect(
    aes(
      xmin = est_unadjusted$conf.low[4],
      xmax = est_unadjusted$conf.high[4],
      ymin = 3.45,
      ymax = 2.55
    ),
    color = alpha("gray50", 0),
    fill = alpha("gray50", 0.01)
  ) +
  geom_rect(
    aes(
      xmin = est_unadjusted$conf.low[3],
      xmax = est_unadjusted$conf.high[3],
      ymin = 4.45,
      ymax = 3.55
    ),
    color = alpha("gray50", 0),
    fill = alpha("gray50", 0.01)
  ) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
    width = 0,
    position = position_dodge(width_par), linewidth = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Anchor main" = "darkcyan",
      "Anchor alphabet" = alpha("darkcyan", 0.5),
      "Anchor exact" = alpha("darkcyan", 0.5),
      "Repeat" = alpha("dimgray", 0.5),
      "Attention I" = alpha("maroon", 0.5),
      "Attention II" = alpha("maroon", 0.5)
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
  xlim(0.8, 4.1) +
  ylab("") +
  xlab("Average rank") +
  theme_bw() +
  theme(
    legend.position = "null",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    text = element_text(size = 10)
  )

ggpubr::ggarrange(
  pdf_default(a) + theme(legend.position = "none"),
  pdf_default(b) + theme(legend.position = "none") +
    ## Add a legend for geom_rect within the plot
    ## Left-align the text, family = CM Roman
    annotate(
      "text",
      x = 1, y = 1, size = 2.5, hjust = 0, family = "CM Roman",
      label = paste0(
        "Gray region = \n",
        "95% CIs of unadjusted \n",
        "(raw data) estimates"
      )
    ),
  ncol = 1, heights = c(0.4, 1)
)

## Figure C.16, formerly weight-avg-rank-multiple-anchors.pdf
ggsave(
  here("fig", "AtsusakaKimFigC16.pdf"),
  width = 5, height = 5
)
