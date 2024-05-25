# Setup ========================================================================
source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main %>%
  select(contains("app_polar"), contains("anc_polar"), anc_correct_polar) %>%
  select(-contains("repeat"), -contains("recorded"), -contains("_rnd"))

item_list <- data.frame(
  item = c(
    "Politicians", "Print Media and TV", "Social Media",
    "Interest Groups", "Citizens"
  ),
  variable = paste("app_polar", 1:5, sep = "_")
)

# Test the function ============================================================
test <- imprr(
  data = main,
  J = 5,
  main_q = "app_polar",
  anchor_q = "anc_polar",
  anc_correct = "anc_correct_polar",
  n_bootstrap = 500
)

# Validity check via average ranks =============================================
dt_w <- main %>%
  mutate(ranking = app_polar) %>%
  select(
    app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5,
    ranking
  ) %>%
  left_join(test$weights, by = "ranking")

# Validity check via average ranks =============================================

## Raw data ====================================================================
id1 <- lm_robust(app_polar_1 ~ 1, dt_w) %>% tidy()
id2 <- lm_robust(app_polar_2 ~ 1, dt_w) %>% tidy()
id3 <- lm_robust(app_polar_3 ~ 1, dt_w) %>% tidy()
id4 <- lm_robust(app_polar_4 ~ 1, dt_w) %>% tidy()
id5 <- lm_robust(app_polar_5 ~ 1, dt_w) %>% tidy()

rbind(id1, id2, id3, id4, id5) %>%
  mutate(
    mean = round(estimate, d = 2),
    lower = round(conf.low, d = 2),
    upper = round(conf.high, d = 2),
    item = outcome,
    qoi = "average rank"
  ) %>%
  select(item, qoi, mean, lower, upper) %>%
  mutate(method = "Raw data") -> result_raw

## Same as the following
result_raw
avg_rank(main, "app_polar", items = item_list$item)

## Based on IPW estimator =======================================================
id1 <- lm_robust(app_polar_1 ~ 1, dt_w, weights = w) %>% tidy()
id2 <- lm_robust(app_polar_2 ~ 1, dt_w, weights = w) %>% tidy()
id3 <- lm_robust(app_polar_3 ~ 1, dt_w, weights = w) %>% tidy()
id4 <- lm_robust(app_polar_4 ~ 1, dt_w, weights = w) %>% tidy()
id5 <- lm_robust(app_polar_5 ~ 1, dt_w, weights = w) %>% tidy()

rbind(id1, id2, id3, id4, id5) %>%
  mutate(
    mean = round(estimate, d = 2),
    lower = round(conf.low, d = 2),
    upper = round(conf.high, d = 2),
    item = outcome,
    qoi = "average rank"
  ) %>%
  select(item, qoi, mean, lower, upper) %>%
  mutate(method = "IPW") -> result_ipw

result_ipw
avg_rank(dt_w, raw = FALSE, weight = "w", items = item_list, round = 2)

## Based on direct bias-correction =============================================
test$qoi %>%
  filter(qoi == "average rank") %>%
  select(mean, lower, upper) %>%
  mutate(method = "Direct") -> result_direct

result_raw
result_ipw
result_direct
g <- rbind(result_raw, result_ipw, result_direct)

ggplot(g, aes(x = mean, y = item)) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "gray50") +
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
      alpha("deeppink4", 1),
      alpha("deepskyblue4", 1),
      alpha("slategray", 1)
    ),
    name = "method", labels = c("Direct", "IPW", "Raw data")
  ) +
  scale_shape_manual(
    name = "method", labels = c("Direct", "IPW", "Raw data"),
    values = c(16, 17, 15)
  ) +
  scale_linetype_manual(
    name = "method", labels = c("Direct", "IPW", "Raw data"),
    values = c(1, 2, 3)
  ) +
  xlim(1.8, 5) +
  xlab("average rank") +
  theme_bw()

ggsave(
  here("fig", "testing_imprr_v2_affective.pdf"),
  width = 9, height = 3
)

qoi <- test$qoi
head(qoi, n = 20)
