source(here::here("R", "utilities.R"))

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
## Median 9.6 and Mean 11.6
summary(main$duration_in_seconds) / 60

## I find it implausible to have properly answered this survey under 5 min
## but will keep them for now
sum(main$duration_in_seconds < 60 * 5)
main %>%
  filter(duration_in_seconds < 60 * 5) %>%
  View()

## Recaptcha score
## Filtering for observations with recaptcha score < 0.8
## One NA value here; not sure why
summary(main$q_recaptcha_score)
sum(main$q_recaptcha_score < 0.8, na.rm = TRUE)
## 31 out of 81 (38.3%)
sum(main$q_recaptcha_score < 1, na.rm = TRUE)

## Ternovski attention filter --------------------------------------------------
## 13 out of 81 (16.0%)
sum(main$ternovsky_screener2 != "Extremely interested,Very interested")

## Berinsky attention filter ---------------------------------------------------
## Berinsky filter more robust
## 29 out of 81 (35.8%)
sum(main$berinsky_screener != "California,New York")

## Filtering after sanity checks -----------------------------------------------
main <- main %>%
  filter(q_recaptcha_score >= 0.8)
