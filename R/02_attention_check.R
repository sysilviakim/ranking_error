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

## Pairwise correlation matrix -------------------------------------------------
cor_matrix <- cor(temp, use = "pairwise.complete.obs") %>%
  round(., digits = 2)

print(
  cor_matrix %>%
    xtable(
      caption = "Pairwise Correlation Between Checks for Random Responses",
      label = "tab:cor-random-checks",
      align = "lrrrrrr"
    ),
  file = here("tab", "cor_random_check.tex"),
  floating = FALSE,
  booktabs = TRUE
)

## Corrplot --------------------------------------------------------------------
# corrplot(
#   cor_matrix, method = "color", type = "lower",
#   tl.cex = 0.8, tl.col = "black", tl.srt = 0, tl.pos = "lt",
#   addCoef.col = "black", number.cex = 0.8
# )

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
    size = 3, family = "CM Roman"
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
## in the latency of the main ranking question?
temp <- left_join(main, df_list$raw, by = "response_id")

temp %>%
  group_by(ternovski_fail_label) %>%
  summarise(mean(page_app_identity_page_timing))

# Pattern comparison by attention checks =======================================
print(
  ggarrange(
    plotlist = berinsky_plots$identity %>%
      map(
        ~ .x +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      ),
    ncol = 1
  )
)
ggsave(
  here("fig", "berinsky_fail_recorded_identity.pdf"),
  width = 6, height = 3
)

print(
  ggarrange(
    plotlist = ternovski_plots$identity %>%
      map(
        ~ .x +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      ),
    ncol = 1
  )
)
ggsave(
  here("fig", "ternovski_fail_recorded_identity.pdf"),
  width = 6, height = 3
)