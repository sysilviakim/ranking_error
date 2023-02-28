source(here::here("R", "utilities.R"))
# df_list <- qualtrics_import("pretest-01-sanitized-numeric.csv")
df_list <- qualtrics_import("pretest-02.csv")
main <- df_list$main

# No-context questions =========================================================
x3 <- main$no_context_3_opts_1
x4a <- main$no_context_4_opts_1

## For a chi-squared test, exclude partial rankings
## 16 out of 100
# sum(grepl("9", x3))
## 15 out of 100
# sum(grepl("9", x4a))

tab3 <- table(x3[!grepl("9", x3)])
tab4a <- table(x4a[!grepl("9", x4a)])

round(prop.table(tab3) * 100, digits = 1)
#  123  132  213  231  312  321
# 10.7 16.7 14.3 17.9 16.7 23.8 (pretest 01)
#  8.9 15.6 17.8 17.8 26.7 13.3 (pretest 02)

## Definitely respondent fatigue; no longer has that 1-2-3-4 skew
## 4-option constrained to top 3 is similarly skewed towards 1-2-3-missing
round(prop.table(tab4a) * 100, digits = 1)
# 1234 1243 1324 1342 1423 2134 2143 2314 2341 2413 2431 3124 
#  2.2  6.5  4.3  8.7  2.2  2.2  4.3  4.3  6.5  2.2  4.3  2.2 
# 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321 
#  2.2  2.2  4.3  2.2  6.5  2.2  8.7  6.5  6.5  2.2  6.5

## This assertion is necessary but does not apply to this small pretest sample
## assert_that(length(tab4a) == 24)
## Therefore, supplement missing permutations
tab4 <- permn_augment(tab4a)

## Chi-square test and power test ----------------------------------------------
chisq_power(tab3) ## p-value = 0.4493, ES = 0.3243, need N = 188+
chisq_power(tab4) ## p-value = 0.911,  ES = 0.5619, need N = 102+

## Visualize -------------------------------------------------------------------
### 3-option
temp <- table_to_tibble(tab3)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-nocontext-3opt.pdf"), width = 4.5, height = 2.8)

### 4-option
temp <- table_to_tibble(tab4)
plot_nolegend(pdf_default(plot_dist_ranking(temp))) ## , ylim = 0.65
ggsave(here("fig", "pretest-nocontext-4opt.pdf"), width = 7.5, height = 2.8)

# Weak context questions =======================================================
## Average of transitivity violations
# mean(main$transitivity_violate, na.rm = TRUE)
# [1] 0.35

## What if we drop nonsincere/inattentive respondents?
# main %>%
#   group_by(ns_symbol_3opts, ns_symbol_4opts) %>%
#   rename(tv = transitivity_violate) %>%
#   summarise(
#     transitivity_violate = mean(
#       tv[!is.infinite(tv) & !is.nan(tv)],
#       na.rm = TRUE
#     )
#   )
#   ns_symbol_3opts ns_symbol_4opts transitivity_violate
#             <dbl>           <dbl>                <dbl>
# 1               0               0                0.097
# 2               0               1                0.222 
# 3               1               0                0.077
# 4               1               1                0.617 
