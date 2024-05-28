# Setup ========================================================================
source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

data <- df_list$main %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity
  )

# Testing the function =========================================================
test <- imprr_weights(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q = "anc_identity",
  anc_correct = "anc_correct_identity"
)
print(test)

# Merging the weights ==========================================================
dt_w <- main %>%
  mutate(ranking = app_identity) %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    ranking
  ) %>%
  left_join(test$weights, by = "ranking")

# Validity check via average ranks =============================================

## Raw data ====================================================================
id1 <- lm_robust(app_identity_1 ~ 1, dt_w) %>% tidy()
id2 <- lm_robust(app_identity_2 ~ 1, dt_w) %>% tidy()
id3 <- lm_robust(app_identity_3 ~ 1, dt_w) %>% tidy()
id4 <- lm_robust(app_identity_4 ~ 1, dt_w) %>% tidy()

rbind(id1, id2, id3, id4) %>%
  mutate(
    mean = round(estimate, d = 20),
    lower = round(conf.low, d = 20),
    upper = round(conf.high, d = 20),
    item = outcome,
    qoi = "average rank"
  ) %>%
  select(item, qoi, mean, lower, upper) %>%
  mutate(method = "Raw data") -> result_raw

## Based on IPW estimator =======================================================
id1 <- lm_robust(app_identity_1 ~ 1, dt_w, weights = w) %>% tidy()
id2 <- lm_robust(app_identity_2 ~ 1, dt_w, weights = w) %>% tidy()
id3 <- lm_robust(app_identity_3 ~ 1, dt_w, weights = w) %>% tidy()
id4 <- lm_robust(app_identity_4 ~ 1, dt_w, weights = w) %>% tidy()

rbind(id1, id2, id3, id4) %>%
  mutate(
    mean = round(estimate, d = 20),
    lower = round(conf.low, d = 20),
    upper = round(conf.high, d = 20),
    item = outcome,
    qoi = "average rank"
  ) %>%
  select(item, qoi, mean, lower, upper) %>%
  mutate(method = "IPW") -> result_ipw

# Check
result_raw
result_ipw

# Combine results
g <- rbind(result_raw, result_ipw)

# Visualize
ggplot(g, aes(x = mean, y = item)) +
  geom_point(aes(color = method, shape = method),
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(
      xmin = lower, xmax = upper,
      color = method, linetype = method
    ),
    width = 0,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(
    values = c(
      alpha("deepskyblue4", 1),
      alpha("slategray", 1)
    ),
    name = "method", labels = c("IPW", "Raw data")
  ) +
  scale_shape_manual(
    name = "method", labels = c("IPW", "Raw data"),
    values = c(17, 15)
  ) +
  scale_linetype_manual(
    name = "method", labels = c("IPW", "Raw data"),
    values = c(2, 3)
  ) +
  xlim(1.5, 3.5) +
  xlab("average rank") +
  theme_bw()

ggsave(here::here("fig", "testing_imprr_weights.pdf"), width = 9, height = 3)
