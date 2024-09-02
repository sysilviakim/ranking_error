source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main %>%
  mutate(
    repeated_correct = case_when(
      repeat_identity == 1 ~ "Failed",
      repeat_identity == 0 ~ "Passed"
    )
  )

# No-context questions (Appendix) ==============================================
## Create dist. tables and supplement missing permutations ---------------------
tab3r <- table(main$no_context_3_recorded) %>% permn_augment()
tab4r <- table(main$no_context_4_recorded) %>% permn_augment()

## Distributions ---------------------------------------------------------------
round(prop.table(tab3r) * 100, digits = 1)
#  123  132  213  231  312  321
# 67.8  7.6 10.7  4.6  3.0  6.3

round(prop.table(tab4r) * 100, digits = 1)
# 1234 1243 1324 1342 1423 1432 2134 2143 2314 2341 2413 2431
# 57.9  2.2  7.4  1.5  1.3  1.3  5.9  2.2  3.0  0.6  1.3  0.9
# 3124 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321
#  0.9  0.7  1.5  2.4  0.2  0.4  0.6  0.4  1.3  1.5  0.0  4.8

## Chi-square and power tests --------------------------------------------------
chisq_power(tab3r) ## p-value < 2.2e-16, ES = 1.3791, need N = 11+
chisq_power(tab4r) ## p-value < 2.2e-16, ES = 2.7244, need N = 5+

## Visualize -------------------------------------------------------------------
## 3-option, recorded
temp <- table_to_tibble(tab3r)
plot_nolegend(
  pdf_default(plot_dist_ranking(temp, ylim = .7, family = "Verdana"))
)

## Figure B.4(a), formerly nocontext3-recorded.pdf
ggsave(
  here("fig", "AtsusakaKimFigB4a.pdf"),
  width = 4.5, height = 2.8
)

## 4-option, recorded
temp <- table_to_tibble(tab4r)
plot_nolegend(
  pdf_default(plot_dist_ranking(temp, ylim = .7, family = "Verdana"))
)

## Figure B.4(b), formerly nocontext4-recorded.pdf
ggsave(
  here("fig", "AtsusakaKimFigB4b.pdf"),
  width = 7.5, height = 2.8
)

# Main application and anchor questions ========================================
## Main ranking question of interest (identity) --------------------------------
tab <- table(main$app_identity_recorded) %>% permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## X-sq = 121.593, p-value = 2.149e-15, ES = 0.3352, N = 287+
temp <- table_to_tibble(tab)
p1 <- plot_nolegend(
  pdf_default(
    plot_dist_ranking(temp, ylim = .15, family = "Verdana", fill = "darkcyan")
  )
)
p1

## Figure 6(a), formerly main-identity-recorded.pdf
ggsave(
  here("fig", "AtsusakaKimFig6a.pdf"),
  width = 7.5, height = 2.5
)

## Main anchor -----------------------------------------------------------------
## Subgroup: those who passed the main anchor
tab <- main %>%
  filter(anc_correct_identity == 1) %>%
  {table(.$anc_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.1066, ES = 0.2050, N = 767+
temp <- table_to_tibble(tab)
pass <-
  plot_dist_ranking(temp, ylim = .15, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure 6(b), formerly main-anchor-recorded-passers.pdf
ggsave(
  here("fig", "Fig6b.pdf"),
  width = 7.5, height = 2.5
)

pass <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.12(a), formerly main-anchor-recorded-passers-ylim-20.pdf
ggsave(
  here("fig", "AtsusakaKimFigC12a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the main anchor
tab <- main %>%
  filter(anc_correct_identity == 0) %>%
  {table(.$anc_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 5.793e-13, ES = 0.5737, N = 98+
temp <- table_to_tibble(tab)
fail <-
  plot_dist_ranking(temp, ylim = .15, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure 6(c), formerly main-anchor-recorded-failers.pdf
ggsave(
  here("fig", "AtsusakaKimFig6c.pdf"),
  width = 7.5, height = 2.5
)

fail <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.12(b), formerly main-anchor-recorded-failers-ylim-20.pdf
ggsave(
  here("fig", "AtsusakaKimFigC12b.pdf"),
  width = 7.5, height = 2.5
)

## Alphabet anchor -------------------------------------------------------------
## Subgroup: those who passed the alphabet anchor
tab <- main %>%
  filter(anc_correct_id_alphabet == 1) %>%
  {table(.$anc_id_alphabet_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.8084, ES = 0.2685, N = 447+
temp <- table_to_tibble(tab)
pass <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.13(a), formerly alphabet-anchor-recorded-passers.pdf
ggsave(
  here("fig", "AtsusakaKimFigC13a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the alphabet anchor
tab <- main %>%
  filter(anc_correct_id_alphabet == 0) %>%
  {table(.$anc_id_alphabet_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.05695, ES = 0.3390, N = 280+
temp <- table_to_tibble(tab)
fail <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.13(b), formerly alphabet-anchor-recorded-failers.pdf
ggsave(
  here("fig", "AtsusakaKimC13b.pdf"),
  width = 7.5, height = 2.5
)

## Exact anchor ----------------------------------------------------------------
## Subgroup: those who passed the exact anchor
tab <- main %>%
  filter(anc_correct_id_exact == 1) %>%
  {table(.$anc_id_exact_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.4055, ES = 0.2870, N = 391+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.14(a), formerly exact-anchor-recorded-passers.pdf
ggsave(
  here("fig", "AtsusakaKimFigC14a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the exact anchor
tab <- main %>%
  filter(anc_correct_id_exact == 0) %>%
  {table(.$anc_id_exact_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 6.595e-08, ES = 0.5543, N = 105+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.14(b), formerly exact-anchor-recorded-failers.pdf
ggsave(
  here("fig", "exact-anchor-recorded-failers.pdf"),
  width = 7.5, height = 2.5
)

## Repeated question -----------------------------------------------------------
## Subgroup: those who passed the repeated question
tab <- main %>%
  filter(repeated_correct == "Passed") %>%
  {table(.$app_identity_repeat_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.01503, ES = 0.3332, N = 290+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.15(a), formerly repeated-recorded-passers.pdf
ggsave(
  here("fig", "AtsusakaKimFigC15a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the repeated question
tab <- main %>%
  filter(repeated_correct == "Failed") %>%
  {table(.$app_identity_repeat_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 3.215e-12, ES = 0.7407, N = 58+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.15(b), formerly repeated-recorded-failers.pdf
ggsave(
  here("fig", "AtsusakaKimFigC15b.pdf"),
  width = 7.5, height = 2.5
)

# Six alternatives vs. main ranking question ===================================
## Note that this check has nothing to do with our actual assumption
## because we're not asking that respondents have the same type of response
## (whether random or nonrandom) to the six alternatives question and the main

## Ternovski attention check (Attention I): main question ----------------------
## You can't actually do the uniformity test with the attention check 
## the above way, because the attention check is not in the format of a ranking
## unless you're going to use the main identity ranking?

## Subgroup: those who passed the ternovski attention check
tab <- main %>%
  filter(ternovski_fail_label == "Passed") %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 9.101e-11, ES = 0.3194, N = 316+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))
ggsave(
  here("fig", "ternovski-attention-recorded-passers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the exact anchor
tab <- main %>%
  filter(ternovski_fail_label == "Failed") %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 8.368e-05, ES = 0.6240, N = 82+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))
ggsave(
  here("fig", "ternovski-attention-recorded-failers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Berinsky attention check (Attention I): main question -----------------------
## Same principle applies.
## Subgroup: those who passed the ternovski attention check
tab <- main %>%
  filter(berinsky_fail_label == "Passed") %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 5.89e-09, ES = 0.3022, N = 353+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))
ggsave(
  here("fig", "berinsky-attention-recorded-passers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the exact anchor
tab <- main %>%
  filter(berinsky_fail_label == "Failed") %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 6.087e-11, ES = 0.7832, N = 52+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))
ggsave(
  here("fig", "berinsky-attention-recorded-failers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Repeated question: main question --------------------------------------------
## Subgroup: those who passed the repeated question
tab <- main %>%
  filter(repeated_correct == "Passed") %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.003545, ES = 0.3546, N = 256+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))
ggsave(
  here("fig", "repeated-recorded-passers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the repeated question
tab <- main %>%
  filter(repeated_correct == "Failed") %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.0007637, ES = 0.5175, N = 120+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))
ggsave(
  here("fig", "repeated-recorded-failers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Main anchor -----------------------------------------------------------------
## Subgroup: those who passed the main anchor
tab <- main %>%
  filter(anc_correct_identity == 1) %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 5.399e-06, ES = 0.2953, N = 370+
temp <- table_to_tibble(tab)
pass <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))
ggsave(
  here("fig", "main-anchor-recorded-passers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the main anchor
tab <- main %>%
  filter(anc_correct_identity == 0) %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 8.767e-13, ES = 0.5710, N = 98+
temp <- table_to_tibble(tab)
fail <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))
ggsave(
  here("fig", "main-anchor-recorded-failers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Alphabet anchor -------------------------------------------------------------
## Subgroup: those who passed the alphabet anchor
tab <- main %>%
  filter(anc_correct_id_alphabet == 1) %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.01003, ES = 0.4200, N = 182+
temp <- table_to_tibble(tab)
pass <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))
ggsave(
  here("fig", "alphabet-anchor-recorded-passers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the alphabet anchor
tab <- main %>%
  filter(anc_correct_id_alphabet == 0) %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 6.849e-10, ES = 0.5471, N = 107+
temp <- table_to_tibble(tab)
fail <-
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))
ggsave(
  here("fig", "exact-anchor-recorded-failers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Exact anchor ----------------------------------------------------------------
## Subgroup: those who passed the exact anchor
tab <- main %>%
  filter(anc_correct_id_exact == 1) %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.2879, ES = 0.3005, N = 357+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(pass))
ggsave(
  here("fig", "exact-anchor-recorded-passers-main-question.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the exact anchor
tab <- main %>%
  filter(anc_correct_id_exact == 0) %>%
  {table(.$app_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 1.974e-05, ES = 0.4939, N = 132+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "Verdana", fill = "dimgray")
plot_nolegend(pdf_default(fail))
ggsave(
  here("fig", "alphabet-anchor-recorded-failers-main-question.pdf"),
  width = 7.5, height = 2.5
)
