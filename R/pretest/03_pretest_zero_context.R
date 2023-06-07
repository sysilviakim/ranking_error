source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# No-context questions =========================================================
x3 <- main$no_context_3_options
x4 <- main$no_context_4_options

## For a chi-squared test, exclude partial rankings, but in v2, none such
tab3 <- table(x3[!grepl("9", x3)])
tab4 <- table(x4[!grepl("9", x4)])

round(prop.table(tab3) * 100, digits = 1)
#  123  132  213  231  312  321
#  8.3 22.9 12.5 14.6 16.7 25.0

round(prop.table(tab4) * 100, digits = 1)
# 1234 1243 1324 1342 1423 1432 2134 2314 2341 2431 3124 3142 3214 3241 3412 
#    4   12   12    4    2    6    6    2    8    4   10    4    4    2    4 
# 3421 4123 4132 4312 4321 
#    2    2    6    2    4

## This assertion is necessary but does not apply to this small pretest sample
## assert_that(length(tab4) == 24)
## Therefore, supplement missing permutations
tab4 <- permn_augment(tab4)

## Chi-square test and power test ----------------------------------------------
chisq_power(tab3) ## p-value = 0.3313, ES = 0.3461, need N = 165+
chisq_power(tab4) ## p-value = 0.0724, ES = 0.8188, need N =  48+

## Visualize -------------------------------------------------------------------
### 3-option
temp <- table_to_tibble(tab3)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest03-nocontext-3opt.pdf"), width = 4.5, height = 2.8)

### 4-option
temp <- table_to_tibble(tab4)
plot_nolegend(pdf_default(plot_dist_ranking(temp))) ## , ylim = 0.65
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
