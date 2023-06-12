source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# No-context questions =========================================================
x3 <- main$no_context_3_options_observed
x4 <- main$no_context_4_options_observed

## For a chi-squared test, exclude partial rankings, but in v2, none such
tab3 <- table(x3[!grepl("9", x3)])
tab4 <- table(x4[!grepl("9", x4)])

round(prop.table(tab3) * 100, digits = 1)
#  123  132  213  231  312  321 
# 54.2  6.2 20.8  8.3  2.1  8.3 

round(prop.table(tab4) * 100, digits = 1)
# 1234 1324 1342 1423 1432 2134 2143 2314 3124 3142 3214 3241 4132 4213 4231 4321 
#   42    4    2    4    2    4    4    4    4    2    6    2    2    2    6   10

## This assertion is necessary but does not apply to this small pretest sample
## assert_that(length(tab4) == 24)
## Therefore, supplement missing permutations
tab4 <- permn_augment(tab4)

## Chi-square test and power test ----------------------------------------------
chisq_power(tab3) ## p-value = 1.862e-10, ES = 1.0631, need N = 18+
chisq_power(tab4) ## p-value < 2.2e-16, ES = 1.9835, need N = 9+

## Visualize -------------------------------------------------------------------
### 3-option
temp <- table_to_tibble(tab3)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .55)))
ggsave(here("fig", "pretest03-nocontext-3opt.pdf"), width = 4.5, height = 2.8)

### 4-option
temp <- table_to_tibble(tab4)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .55)))
ggsave(here("fig", "pretest03-nocontext-4opt.pdf"), width = 7.5, height = 2.8)

## Is it attention? ------------------------------------------------------------
temp <- main %>%
  group_by(ternovsky_fail) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$ternovsky_fail[1]) %>% unlist()) %>%
  map(
    ~ round(
      permn_augment(prop.table(table(.x$no_context_3_options)), J = 3) * 100,
      digits = 1
    )
  )
temp

# $`0`
# 123  132  213  231  312  321 
# 8.1 21.6 16.2 13.5 18.9 21.6 
# 
# $`1` ---> interesting. So the failures of Ternovski went for 3-2-1
# 123  132  231  312  321  213 
# 9.1 27.3 18.2  9.1 36.4  0.0 

# Effect size is 0.2873 for 0
# Effect size is 0.7329 for 1 (much less uniform)
temp %>%
  map(chisq_power)

temp <- main %>%
  group_by(berinsky_fail) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$berinsky_fail[1]) %>% unlist()) %>%
  map(
    ~ round(
      permn_augment(prop.table(table(.x$no_context_3_options)), J = 3) * 100,
      digits = 1
    )
  )
temp
# $`0`
# 123  132  213  231  312  321 
# 12.1 24.2 18.2 12.1 18.2 15.2 
# 
# $`1`
# 132  231  312  321  123  213 
# 20.0 20.0 13.3 46.7  0.0  0.0

# Effect size is 0.2514 for 0
# Effect size is 0.9459 for 1 (much less uniform)
temp %>%
  map(chisq_power)
