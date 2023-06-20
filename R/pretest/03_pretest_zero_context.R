source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# No-context questions =========================================================
x3o <- main$no_context_3_options_observed
x4o <- main$no_context_4_options_observed
x3r <- main$no_context_3_options
x4r <- main$no_context_4_options

## For a chi-squared test, exclude partial rankings, but in v2, none such
tab3o <- table(x3o[!grepl("9", x3o)])
tab4o <- table(x4o[!grepl("9", x4o)])
tab3r <- table(x3r[!grepl("9", x3r)])
tab4r <- table(x4r[!grepl("9", x4r)])

round(prop.table(tab3o) * 100, digits = 1)
#  123  132  213  231  312  321
# 54.2  6.2 20.8  8.3  2.1  8.3

round(prop.table(tab4o) * 100, digits = 1)
# 1234 1324 1342 1423 1432 2134 2143 2314 3124 3142 3214 3241 4132 4213 4231 4321
#   42    4    2    4    2    4    4    4    4    2    6    2    2    2    6   10

## This assertion is necessary but does not apply to this small pretest sample
## assert_that(length(tab4) == 24)
## Therefore, supplement missing permutations
tab4o <- permn_augment(tab4o)
tab4r <- permn_augment(tab4r)

## Chi-square test and power test ----------------------------------------------
chisq_power(tab3o) ## p-value = 1.862e-10, ES = 1.0631, need N = 18+
chisq_power(tab4o) ## p-value < 2.2e-16, ES = 1.9835, need N = 9+
chisq_power(tab3r) ## p-value = 0.3313, ES = 0.3461, need N = 165+
chisq_power(tab4r) ## p-value = 0.0724, ES = 0.8188, need N = 48+

## Visualize -------------------------------------------------------------------
### 3-option, observed
temp <- table_to_tibble(tab3o)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .55)))
ggsave(
  here("fig", "pretest", "pretest03-nocontext-3opt-observed.pdf"),
  width = 4.5, height = 2.8
)

### 4-option, observed
temp <- table_to_tibble(tab4o)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .55)))
ggsave(
  here("fig", "pretest", "pretest03-nocontext-4opt-observed.pdf"),
  width = 7.5, height = 2.8
)

### 3-option, resulting
temp <- table_to_tibble(tab3r)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .55))) + 
  xlab("Resulting Ranking")
ggsave(
  here("fig", "pretest", "pretest03-nocontext-3opt-resulting.pdf"),
  width = 4.5, height = 2.8
)

### 4-option, observed
temp <- table_to_tibble(tab4r)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .55))) + 
  xlab("Resulting Ranking")
ggsave(
  here("fig", "pretest", "pretest03-nocontext-4opt-resulting.pdf"),
  width = 7.5, height = 2.8
)

## Is it attention? ------------------------------------------------------------
temp <- main %>%
  group_by(ternovski_fail) %>%
  group_split() %>%
  `names<-`({
    .
  } %>% map(~ .x$ternovski_fail[1]) %>% unlist()) %>%
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
  `names<-`({
    .
  } %>% map(~ .x$berinsky_fail[1]) %>% unlist()) %>%
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
