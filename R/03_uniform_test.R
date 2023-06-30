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
# 15.9 15.5 21.6 13.2 17.0 16.8 

round(prop.table(tab3r) * 100, digits = 1)
#  123  132  213  231  312  321 
# 63.5  9.2 11.3  5.9  3.1  6.9 

round(prop.table(tab4o) * 100, digits = 1)
# 1234 1243 1324 1342 1423 1432 2134 2143 2314 2341 2413 2431 
#  5.8  4.5  4.7  4.3  3.9  4.9  3.0  4.5  5.1  3.6  4.3  4.3 
# 3124 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321 
#  3.4  3.0  4.1  4.1  3.9  3.6  4.3  3.4  3.6  4.7  4.5  4.5

round(prop.table(tab4r) * 100, digits = 1)
# 1234 1243 1324 1342 1423 1432 2134 2143 2314 2341 2413 2431 
# 58.2  2.1  6.4  1.7  1.1  1.3  4.9  2.6  2.8  0.4  1.5  1.5 
# 3124 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321 
#  1.1  0.6  1.5  2.8  0.2  0.4  0.4  0.4  1.3  1.5  0.0  5.1

## Chi-square and power tests --------------------------------------------------
chisq_power(tab3o) ## p-value = 0.05264, ES = 0.1514, need N = 863+
chisq_power(tab3r) ## p-value < 2.2e-16, ES = 1.2666, need N = 13+
chisq_power(tab4o) ## p-value = 0.9799,  ES = 0.1556, need N = 1,334+ 
chisq_power(tab4r) ## p-value < 2.2e-16, ES = 2.7334, need N = 3+

## Visualize -------------------------------------------------------------------
### 3-option, observed
temp <- table_to_tibble(tab3o)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .65))) + 
  xlab("Observed Rankings")
ggsave(
  here("fig", "nocontext3-observed.pdf"),
  width = 4.5, height = 2.8
)

### 4-option, observed
temp <- table_to_tibble(tab4o)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .65))) + 
  xlab("Observed Rankings")
ggsave(
  here("fig", "nocontext4-observed.pdf"),
  width = 7.5, height = 2.8
)

### 3-option, recorded
temp <- table_to_tibble(tab3r)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .65)))
ggsave(
  here("fig", "nocontext3-recorded.pdf"),
  width = 4.5, height = 2.8
)

### 4-option, recorded
temp <- table_to_tibble(tab4r)
plot_nolegend(pdf_default(plot_dist_ranking(temp, ylim = .65)))
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

