source(here::here("R", "pretest", "v1", "utilities.R"))

# Data import ==================================================================
## Lucid Theorem
df_raw <- read_csv(here("data", "raw", "pretest-02.csv")) %>%
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
  select(response_id, contains("timing"), -contains("time"))

main <- df_raw %>%
  select(-contains("timing"), -contains("time"))

# Sanity checks ================================================================
## Plausible IP address, captcha scores, and survey duration -------------------
## Is there an overlap in IP addresses? No? Great.
assert_that(!any(duplicated(main$ip_address)))

## What is the average duration?
## Median 10.0 and Mean 12.8
summary(main$duration_in_seconds) / 60

## I find it implausible to have properly answered this survey under 5 min
## but will keep them for now
## 12 out of 102 (11.8%)
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
## 39 out of 102 (38.2%)
sum(main$q_recaptcha_score < 1, na.rm = TRUE)

## Filtering after sanity checks -----------------------------------------------
## I guess Lucid already figured out that 2 were bots, so fulfilled up
## to the requested 100 orders
main <- main %>%
  filter(q_recaptcha_score >= 0.8)

## Ternovski attention filter --------------------------------------------------
## 19 out of 100
sum(main$ternovsky_screener2 != "Extremely interested,Very interested")

## Berinsky attention filter ---------------------------------------------------
## Berinsky filter more robust; or is it fatigue (already?!)
## 35 out of 100
sum(main$berinsky_screener != "California,New York")

# Wrangle and unite into ranking pattern =======================================
main <- text_to_item_position(main) %>%
  unite_ranking()

# Lucid-generated demographics sanity check ====================================
## Age
main %>%
  filter(age != age_2) %>%
  select(age, age_2) %>%
  View()

## Some very odd observations
