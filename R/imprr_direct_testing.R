# Setup ========================================================================
source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

data <- df_list$main %>%
  select(app_identity_1, app_identity_2, app_identity_3, app_identity_4,
         anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
         anc_correct_identity)


# Testing the function =========================================================
source(here::here("R", "imprr_direct.R"))

test <- imprr_direct(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 200
)

print(test)



# Validity check via average ranks =============================================

## Raw data ====================================================================
id1 <- lm_robust(app_identity_1 ~ 1, data) %>% tidy()
id2 <- lm_robust(app_identity_2 ~ 1, data) %>% tidy()
id3 <- lm_robust(app_identity_3 ~ 1, data) %>% tidy()
id4 <- lm_robust(app_identity_4 ~ 1, data) %>% tidy()

rbind(id1, id2, id3, id4) %>%
  mutate(mean = round(estimate, d = 2),
         lower = round(conf.low, d = 2),
         upper = round(conf.high, d = 2),
         item = outcome,
         qoi = "average rank") %>%
  select(item, qoi, mean, lower, upper) %>%
  mutate(method = "Raw data") -> result_raw

## Based on direct bias-correction =============================================
test$qoi %>% filter(qoi == "average rank") %>% 
  select(mean, lower, upper) %>%
  mutate(method = "Direct") -> result_direct

result_raw
result_direct


g <- rbind(result_raw, result_direct)

ggplot(g, aes(x = mean, y = item)) +
  geom_point(aes(color = method, shape = method), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper,
                    color = method, linetype = method),
                width = 0,
                position = position_dodge(width = 0.5)) +
  scale_color_manual(
    values = c(
      alpha("deeppink4", 1),
      alpha("slategray", 1)
    ),
    name = "method", labels = c("Direct", "Raw data")
  ) +
  scale_shape_manual(
    name = "method", labels = c("Direct", "Raw data"),
    values = c(16, 15)
  ) +
  scale_linetype_manual(
    name = "method", labels = c("Direct", "Raw data"),
    values = c(1, 3)
  ) +  
  xlim(1.5,3.5) +
  xlab("average rank") +
  theme_bw()



ggsave(here::here("fig", "testing_imprr_direct.pdf"), width = 9, height = 3)


