source(here::here("R", "pretest_import.R"))

# No-context questions =========================================================
x3 <- main$no_context_3_opts_1
x4a <- main$no_context_4_opts_1
x4b <- main$no_context_4_opts_2

round(prop.table(table(x3)) * 100, digits = 1)
round(prop.table(table(x4a)) * 100, digits = 1)
round(prop.table(table(x4b)) * 100, digits = 1)

## For a chi-squared test, exclude partial rankings
## 15 out of 98 (15.3%)
sum(grepl("9", x3))
## 14 out of 98 (14.3%)
sum(grepl("9", x4a))

tab3 <- table(x3[!grepl("9", x3)])
tab4a <- table(x4a[!grepl("9", x4a)])
tab4b <- table(x4b[!grepl("9", x4b)])

round(prop.table(tab3) * 100, digits = 1)
# 123  132  213  231  312  321
# 10.8 16.9 14.5 18.1 15.7 24.1

## Perhaps respondent fatigue?
## 4-option constrained to top 3 is similarly skewed towards 1-2-3-missing
round(prop.table(tab4a) * 100, digits = 1)
# 1234 1243 1324 1423 1432 2134 2143 2314 2341 3124 3214 
# 59.5  1.2  9.5  1.2  2.4  6.0  2.4  1.2  1.2  1.2  1.2 
# 3241 3412 3421 4132 4213 4231 4321 
#  1.2  1.2  1.2  1.2  3.6  2.4  2.4

## This assertion is necessary but does not apply to this small pretest sample
## assert_that(length(tab4a) == 24)
## Therefore, supplement missing permutations
tab4 <- permn_augment(tab4a)

## Chi-square test and power test ----------------------------------------------
chisq_power(tab3) ## p-value = 0.4368, need 340+
chisq_power(tab4) ## p-value < 2.2e-16, N < J! so not meaningful

## Visualize -------------------------------------------------------------------
### 3-option
temp <- table_to_tibble(tab3)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-nocontext-3opt.pdf"), width = 4.5, height = 2.8)

### 4-option
temp <- table_to_tibble(tab4)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = 0.65)))
ggsave(here("fig", "pretest-nocontext-4opt.pdf"), width = 7.5, height = 2.8)

# Weak context questions =======================================================
## Second priority for now
