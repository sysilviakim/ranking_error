source(here::here("R", "utilities.R"))
library(corrplot)

# Import data ==================================================================
## substitute file name when actual data is sent
fname <- "AmericanRanking_June_2023.sav"
if (!file.exists(here("data", "tidy", "df_list.Rda"))) {
  df_list <- yougov_import(fname)
  save(df_list, file = here("data", "tidy", "df_list.Rda"))
} else {
  load(here("data", "tidy", "df_list.Rda"))
}
main <- df_list$main

# What are the raw proportion of passing the test? =============================
prop(main, "ternovski_fail") ## 86.3%
prop(main, "berinsky_fail") ## 85.5%

## Pr(c = 1) (correct answer)
## random_* variables 1 = random, 0 = non-random
prop(main, "random_tate") ## 67.9%
prop(main, "random_identity") ## 69.7%
prop(main, "random_id_alphabet", useNA = "no") ## 43.9%
prop(main, "random_id_exact", useNA = "no") ## 53.4%
prop(main, "random_polar") ## 79.7%
prop(main, "random_esystems") ## 56.3%
prop(main, "random_es_alphabet", useNA = "no") ## 69.0%
prop(main, "random_es_temporal", useNA = "no") ## 59.6%

prop(main, "repeat_tate", useNA = "no") ## 66.3%
prop(main, "repeat_identity", useNA = "no") ## 65.6%
prop(main, "repeat_polar", useNA = "no") ## 53.6%
prop(main, "repeat_esystems", useNA = "no") ## 36.5%

## Unbiased estimator for the proportion of non-random responses?
## 67.9% --> 61.5%
unbiased_correct_prop(
  sum(main$random_tate == 0) / sum(!is.na(main$random_tate)),
  J = 3
)
## 69.7% --> 68.4%
unbiased_correct_prop(
  sum(main$random_identity == 0) / sum(!is.na(main$random_identity)),
  J = 4
)
## 79.7% --> 79.5%
unbiased_correct_prop(
  sum(main$random_polar == 0) / sum(!is.na(main$random_polar)),
  J = 5
)
## 56.3% --> 56.2%
unbiased_correct_prop(
  sum(main$random_esystems == 0) / sum(!is.na(main$random_esystems)),
  J = 6
)

## Exact and alphabet?
unbiased_correct_prop(
  sum(main$random_id_exact == 0, na.rm = TRUE) / 
    sum(!is.na(main$random_id_exact)),
  J = 4
)
unbiased_correct_prop(
  sum(main$random_id_alphabet == 0, na.rm = TRUE) / 
    sum(!is.na(main$random_id_alphabet)),
  J = 4
)

# Correlation between attention filters ========================================
temp <- main %>%
  select(
    `Repeated` = repeat_identity,
    `Attention I` = ternovski_fail,
    `Attention II` = berinsky_fail,
    `Anchor main` = random_identity,
    `Anchor exact` = random_id_exact,
    `Anchor alphabet` = random_id_alphabet
  )

## Corrplot --------------------------------------------------------------------
p <- cor_matrix %>%
  reshape2::melt() %>%
  mutate(
    Var1 = factor(
      Var1,
      levels = c(
        "Repeated", "Attention I", "Attention II", "Anchor main",
        "Anchor exact", "Anchor alphabet"
      )
    ),
    Var2 = factor(
      Var2,
      levels = rev(
        c(
          "Repeated", "Attention I", "Attention II", "Anchor main",
          "Anchor exact", "Anchor alphabet"
        )
      )
    )
  ) %>%
  rename(`Correlation Coefficient` = value) %>%
  ggplot(
    aes(
      x = Var1, y = ordered(Var2, levels = ), fill = `Correlation Coefficient`
    )
  ) +
  geom_tile() +
  scale_fill_distiller(palette = "RdPu", direction = 1) +
  coord_fixed() +
  geom_text(
    aes(label = `Correlation Coefficient`),
    size = 3, family = "Verdana"
  ) +
  labs(x = "", y = "")

pdf_default(p) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(here("fig", "corrplot_random_check.pdf"), width = 5, height = 5)

## Conditional probabilities? --------------------------------------------------
## Between attention filters
cor_and_condprob(main, "ternovski_fail", "berinsky_fail")

## Ternovski test x random responses
cor_and_condprob(main, "ternovski_fail", "random_identity")
cor_and_condprob(main, "ternovski_fail", "random_id_exact")
cor_and_condprob(main, "ternovski_fail", "random_id_alphabet")

## Berinsky test x random responses
cor_and_condprob(main, "berinsky_fail", "random_identity")
cor_and_condprob(main, "berinsky_fail", "random_id_exact")
cor_and_condprob(main, "berinsky_fail", "random_id_alphabet")

## Between random responses (within topic)
cor_and_condprob(main, "random_identity", "random_id_exact")
cor_and_condprob(main, "random_identity", "random_id_alphabet")

## Ternovski test x repeated responses
cor_and_condprob(main, "ternovski_fail", "repeat_identity")

## Berinsky test x repeated responses
cor_and_condprob(main, "berinsky_fail", "repeat_identity")

## Random responses x repeated responses (within topic)
cor_and_condprob(main, "random_identity", "repeat_identity")
cor_and_condprob(main, "random_id_exact", "repeat_identity")
cor_and_condprob(main, "random_id_alphabet", "repeat_identity")

# Quick check: does it matter if anchor comes first or main? ===================
## Absolutely not!
prop.table(table(df_list$raw$hopkins_order_q))
chisq.test(table(df_list$raw$hopkins_order_q, df_list$raw$app_identity_1))
chisq.test(table(df_list$raw$hopkins_order_q, df_list$raw$app_identity_2))
chisq.test(table(df_list$raw$hopkins_order_q, df_list$raw$app_identity_3))
chisq.test(table(df_list$raw$hopkins_order_q, df_list$raw$app_identity_4))

# Latency of the six different screeners =======================================
## Main ranking question
summary(df_list$raw$page_app_identity_page_timing) ## mean 33.5s

## Screeners
summary(df_list$raw$page_app_identity_repeat_page_timing) ## mean 50.1s
summary(df_list$raw$page_attention_check_1_timing) ## mean 23.4s
summary(df_list$raw$page_attention_check_2_timing) # mean 44.3s
summary(df_list$raw$page_hopkins_page_timing) # mean 54.3s
## was marked Identity Hopkins (2023) so this is the main ranking
summary(df_list$raw$page_alpha_page_timing) # mean 22.6s
summary(df_list$raw$page_exact_page_timing) # mean 16.6s

## Depending on failing or passing the screener, is there a difference
## in the latency of the screener itself?
temp <- left_join(main, df_list$raw, by = "response_id")

## 27.7s for failed, 135.6s for passed (p = 0.3305)
t.test(page_app_identity_repeat_page_timing ~ repeat_identity, data = temp)
## 17.4s for failed, 24.4s for passed (p = 0.0601)
t.test(page_attention_check_1_timing ~ ternovski_fail_label, data = temp)
## 43.4s for failed, 44.5s for passed (p = 0.9481)
t.test(page_attention_check_2_timing ~ berinsky_fail_label, data = temp)
## 79.1s for failed, 43.5s for passed (p = 0.4812)
t.test(page_hopkins_page_timing ~ random_identity, data = temp)
## 47.8s for failed, 42.5s for passed (p = 0.3487)
t.test(page_alpha_page_timing ~ random_id_alphabet, data = temp)
## 35.1s for failed, 31.0s for passed (p = 0.3150)
t.test(page_exact_page_timing ~ random_id_exact, data = temp)

## Well... is there for the main ranking question?
## 27.5s for failed, 39.7s for passed (p = 0.0687)
t.test(page_app_identity_page_timing ~ repeat_identity, data = temp)
## 40.0s for failed, 32.5s for passed (p = 0.6192)
t.test(page_app_identity_page_timing ~ ternovski_fail_label, data = temp)
## 23.4s for failed, 35.2s for passed (p = 0.0001842)
t.test(page_app_identity_page_timing ~ berinsky_fail_label, data = temp)
## 31.8s for failed, 34.2s for passed (p = 0.7301)
t.test(page_app_identity_page_timing ~ random_identity, data = temp)
## 30.0s for failed, 44.8s for passed (p = 0.1418)
t.test(page_app_identity_page_timing ~ random_id_alphabet, data = temp)
## 28.8s for failed, 32.1s for passed (p = 0.1169)
t.test(page_app_identity_page_timing ~ random_id_exact, data = temp)

## How about the Wilcoxon rank sum test?
## p-value = 0.08016
wilcox.test(page_app_identity_repeat_page_timing ~ repeat_identity, data = temp)
temp %>%
  group_by(repeat_identity) %>%
  summarise(median(page_app_identity_repeat_page_timing))

## p-value < 2.2e-16
wilcox.test(page_attention_check_1_timing ~ ternovski_fail_label, data = temp)
temp %>%
  group_by(ternovski_fail_label) %>%
  summarise(median(page_attention_check_1_timing))

## p-value < 2.2e-16
wilcox.test(page_attention_check_2_timing ~ berinsky_fail_label, data = temp)
temp %>%
  group_by(berinsky_fail_label) %>%
  summarise(median(page_attention_check_2_timing))

## p = 4.249e-08
wilcox.test(page_hopkins_page_timing ~ random_identity, data = temp)
temp %>%
  group_by(random_identity) %>%
  summarise(median(page_hopkins_page_timing))

## p = 0.3825
wilcox.test(page_alpha_page_timing ~ random_id_alphabet, data = temp)
temp %>%
  group_by(random_id_alphabet) %>%
  summarise(median(page_alpha_page_timing))

## p = 0.006962
wilcox.test(page_exact_page_timing ~ random_id_exact, data = temp)
temp %>%
  group_by(random_id_exact) %>%
  summarise(median(page_exact_page_timing))

## How about the Wilcoxon rank sum test for the main ranking question?
## p = 6.909e-05
wilcox.test(page_app_identity_page_timing ~ repeat_identity, data = temp)
temp %>%
  group_by(repeat_identity) %>%
  summarise(median(page_app_identity_page_timing))

## p = 0.00079
wilcox.test(page_app_identity_page_timing ~ ternovski_fail_label, data = temp)
temp %>%
  group_by(ternovski_fail_label) %>%
  summarise(median(page_app_identity_page_timing))

## p = 6.909e-12
wilcox.test(page_app_identity_page_timing ~ berinsky_fail_label, data = temp)
temp %>%
  group_by(berinsky_fail_label) %>%
  summarise(median(page_app_identity_page_timing))

## p = 1.073e-13
wilcox.test(page_app_identity_page_timing ~ random_identity, data = temp)
temp %>%
  group_by(random_identity) %>%
  summarise(median(page_app_identity_page_timing))

## p = 0.2251
wilcox.test(page_app_identity_page_timing ~ random_id_alphabet, data = temp)
temp %>%
  group_by(random_id_alphabet) %>%
  summarise(median(page_app_identity_page_timing))

## p = 5.202e-06
wilcox.test(page_app_identity_page_timing ~ random_id_exact, data = temp)
temp %>%
  group_by(random_id_exact) %>%
  summarise(median(page_app_identity_page_timing))
