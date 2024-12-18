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
  pdf_default(plot_dist_ranking(temp, ylim = .7, family = "CM Roman"))
)

## Figure B.4(a), formerly nocontext3-recorded.pdf
ggsave(
  here("fig", "FigB4a.pdf"),
  width = 4.5, height = 2.8
)

## 4-option, recorded
temp <- table_to_tibble(tab4r)
plot_nolegend(
  pdf_default(plot_dist_ranking(temp, ylim = .7, family = "CM Roman"))
)

## Figure B.4(b), formerly nocontext4-recorded.pdf
ggsave(
  here("fig", "FigB4b.pdf"),
  width = 7.5, height = 2.8
)
