source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-01-sanitized-numeric.csv")
main <- df_list$main

# No-context questions =========================================================
x3 <- main$no_context_3_opts_1
x4a <- main$no_context_4_opts_1
x4b <- main$no_context_4_opts_2

round(prop.table(table(x3)) * 100, digits = 1)
round(prop.table(table(x4a)) * 100, digits = 1)
round(prop.table(table(x4b)) * 100, digits = 1)

## For a chi-squared test, exclude partial rankings
## 16 out of 100
sum(grepl("9", x3))
## 15 out of 100
sum(grepl("9", x4a))

tab3 <- table(x3[!grepl("9", x3)])
tab4a <- table(x4a[!grepl("9", x4a)])
tab4b <- table(x4b[!grepl("9", x4b)])

round(prop.table(tab3) * 100, digits = 1)
#  123  132  213  231  312  321
# 10.7 16.7 14.3 17.9 16.7 23.8

## Perhaps respondent fatigue?
## 4-option constrained to top 3 is similarly skewed towards 1-2-3-missing
round(prop.table(tab4a) * 100, digits = 1)
# 1234 1243 1324 1423 1432 2134 2143 2314 2341 3124 3214 3241 3412 3421 4132
# 60.0  1.2  9.4  1.2  2.4  5.9  2.4  1.2  1.2  1.2  1.2  1.2  1.2  1.2  1.2
# 4213 4231 4321
#  3.5  2.4  2.4

## This assertion is necessary but does not apply to this small pretest sample
## assert_that(length(tab4a) == 24)
## Therefore, supplement missing permutations
tab4 <- permn_augment(tab4a)

## Chi-square test and power test ----------------------------------------------
chisq_power(tab3) ## p-value = 0.4517,  ES = 0.2369, need N = 352+
chisq_power(tab4) ## p-value < 2.2e-16, ES = 2.838, N < J! so not meaningful

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
## Transitivity test -----------------------------------------------------------
## This is available in the symbols question. 
## For example, if they said they prefer triangle > square > pentagon,
## the addition of hexagon as an option should not reverse square > triangle
## So that if the answer to symbols_3_opts was 321, with the addition of 4,
## we should only allow 4321, 3421, 3241, or 3214. 

## Whopping 35%(!) violated
main <- main %>%
  rowwise() %>%
  mutate(
    transitivity_violate = case_when(
      symbols_4_opts %in% transitivity_pattern(symbols_3_opts) ~ 1,
      TRUE ~ 0
    )
  )
summary(x$transitivity_violate)

## What if we drop nonsincere/inattentive respondents?
main %>%
  mutate(
    nonsincere_symbol_3opts = case_when(
      symbols_3_opts_known == "123" ~ 1,
      TRUE ~ 0
    ),
    nonsincere_symbol_4opts = case_when(
      symbols_4_opts_known == "1234" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(nonsincere_symbol_3opts, nonsincere_symbol_4opts) %>%
  summarise(transitivity_violate = mean(transitivity_violate))

#   nonsincere_symbol_3opts nonsincere_symbol_4opts transitivity_violate
#                     <dbl>                   <dbl>                <dbl>
# 1                       0                       0               0.0968
# 2                       0                       1               0.222 
# 3                       1                       0               0.0769
# 4                       1                       1               0.617 
