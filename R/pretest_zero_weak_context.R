source(here::here("R", "utilities.R"))
library(pwr)

# Data import ==================================================================
## Lucid Theorem
df_raw <- read_csv(here(
  "data", "raw", "pretest.csv"
)) %>%
  clean_names() %>%
  filter(
    start_date != "Start Date" &
      start_date != '{"ImportId":"startDate","timeZone":"America/Denver"}'
  ) %>%
  mutate(
    duration_in_seconds = as.numeric(duration_in_seconds),
    q_recaptcha_score = as.numeric(q_recaptcha_score)
  )

## Separate out timing variables to actual responses
timing <- df_raw %>%
  select(response_id, contains("timing"))

main <- df_raw %>%
  select(-contains("timing"))

# Sanity checks ================================================================
## Plausible IP address, captcha scores, and survey duration -------------------
## Is there an overlap in IP addresses? No? Great.
assert_that(!any(duplicated(main$ip_address)))

## What is the average duration?
## Median 9.9 and Mean 12.0
summary(main$duration_in_seconds) / 60

## I find it implausible to have properly answered this survey under 5 min
## but will keep them for now
## 12 out of 100 (12.0%)
sum(main$duration_in_seconds < 60 * 5)
main %>%
  filter(duration_in_seconds < 60 * 5)

## Recaptcha score
## Filtering for observations with recaptcha score < 0.8
## One NA value here; not sure why
## but if you look at the freeform, definitely a bot
summary(main$q_recaptcha_score)
sum(main$q_recaptcha_score < 0.8, na.rm = TRUE)
sum(is.na(main$q_recaptcha_score), na.rm = TRUE)
## 39 out of 100 (39.0%)
sum(main$q_recaptcha_score < 1, na.rm = TRUE)

## Ternovski attention filter --------------------------------------------------
## 19 out of 100 (19.0%)
sum(main$ternovsky_screener2 != "Extremely interested,Very interested")

## Berinsky attention filter ---------------------------------------------------
## Berinsky filter more robust; or is it fatigue (already?!)
## 36 out of 100 (36.0%)
sum(main$berinsky_screener != "California,New York")

## Filtering after sanity checks -----------------------------------------------
main <- main %>%
  filter(q_recaptcha_score >= 0.8)

# No-context questions =========================================================
## Collapse into ranking pattern -----------------------------------------------
no_context <- main %>%
  mutate(
    across(
      no_context_3_opts_1_1:no_context_4_opts_2_4,
      ~ case_when(.x == "-99" ~ "9", TRUE ~ .x)
    )
  ) %>%
  unite(
    "no_context_3opt",
    sep = "", no_context_3_opts_1_1:no_context_3_opts_1_3
  ) %>%
  unite(
    "no_context_4opt",
    sep = "", no_context_4_opts_1_1:no_context_4_opts_1_4
  ) %>%
  unite(
    "no_context_4opt_limit",
    sep = "", no_context_4_opts_2_1:no_context_4_opts_2_4
  ) %>%
  select(no_context_3opt, no_context_4opt, no_context_4opt_limit, everything())

## Temporary vector
x3 <- no_context$no_context_3opt
x4a <- no_context$no_context_4opt
x4b <- no_context$no_context_4opt_limit

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
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-nocontext-4opt.pdf"), width = 7.5, height = 2.8)

# Weak context questions =======================================================
## Second priority for now
