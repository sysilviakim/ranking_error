source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

# No-context questions =========================================================
## Create dist. tables ---------------------------------------------------------
tab3o <- table(main$no_context_3)
tab3r <- table(main$no_context_3_recorded)
tab4o <- table(main$no_context_4)
tab4r <- table(main$no_context_4_recorded)

## Supplement missing permutations
tab4o <- permn_augment(tab4o)
tab4r <- permn_augment(tab4r)

round(prop.table(tab3o) * 100, digits = 1)
#  123  132  213  231  312  321 
# 14.6 15.6 22.0 14.1 17.6 16.1

round(prop.table(tab3r) * 100, digits = 1)
#  123  132  213  231  312  321 
# 67.8  7.6 10.7  4.6  3.0  6.3

round(prop.table(tab4o) * 100, digits = 1)
# 1234 1243 1324 1342 1423 1432 2134 2143 2314 2341 2413 2431 
#  5.2  4.8  4.4  3.7  4.6  4.6  3.7  4.4  5.2  3.5  3.7  4.6 
# 3124 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321 
#  3.5  3.5  4.6  3.9  3.9  3.7  4.2  3.9  3.5  4.4  4.2  4.2 

round(prop.table(tab4r) * 100, digits = 1)
# 1234 1243 1324 1342 1423 1432 2134 2143 2314 2341 2413 2431 
# 57.9  2.2  7.4  1.5  1.3  1.3  5.9  2.2  3.0  0.6  1.3  0.9 
# 3124 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321 
#  0.9  0.7  1.5  2.4  0.2  0.4  0.6  0.4  1.3  1.5  0.0  4.8

## Chi-square and power tests --------------------------------------------------
chisq_power(tab3o) ## p-value = 0.01803, ES = 0.1590, need N = 782+
chisq_power(tab3r) ## p-value < 2.2e-16, ES = 1.3791, need N = 11+
chisq_power(tab4o) ## p-value = 0.9980,  ES = 0.1232, need N = 2,126+ 
chisq_power(tab4r) ## p-value < 2.2e-16, ES = 2.7244, need N = 5+

## Visualize -------------------------------------------------------------------
### 3-option, observed
temp <- table_to_tibble(tab3o)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .7))) + 
  xlab("Observed Rankings")
ggsave(
  here("fig", "nocontext3-observed.pdf"),
  width = 4.5, height = 2.8
)

### 4-option, observed
temp <- table_to_tibble(tab4o)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .7))) + 
  xlab("Observed Rankings")
ggsave(
  here("fig", "nocontext4-observed.pdf"),
  width = 7.5, height = 2.8
)

### 3-option, recorded
temp <- table_to_tibble(tab3r)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .7)))
ggsave(
  here("fig", "nocontext3-recorded.pdf"),
  width = 4.5, height = 2.8
)

### 4-option, recorded
temp <- table_to_tibble(tab4r)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .7)))
ggsave(
  here("fig", "nocontext4-recorded.pdf"),
  width = 7.5, height = 2.8
)

# Chi-square tests by attention checks =========================================
temp <- main %>%
  group_by(ternovski_fail) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$ternovski_fail[1]) %>% unlist()) %>%
  map(
    ~ round(
      permn_augment(prop.table(table(.x$no_context_3)), J = 3) * 100,
      digits = 1
    )
  )
temp
temp %>% map(chisq_power)

# Chi-square tests by attention checks =========================================
temp <- main %>%
  group_by(ternovski_fail) %>%
  group_split() %>%
  `names<-`({.} %>% map(~ .x$ternovski_fail[1]) %>% unlist()) %>%
  map(
    ~ round(
      permn_augment(prop.table(table(.x$no_context_3)), J = 3) * 100,
      digits = 1
    )
  )
temp
temp %>% map(chisq_power)
