source(here::here("R", "utilities.R"))
# Qualtrics test responses (Jan 22, 2023)

# Data import ==================================================================
test_rep <- read_csv(here(
  "data", "raw", "qualtrics-test-responses",
  "RankingSalience_January 22, 2023_21.05.csv"
)) %>%
  clean_names() %>%
  filter(
    start_date != "Start Date" &
      start_date != '{"ImportId":"startDate","timeZone":"America/Denver"}'
  )

# Pre-pilot ====================================================================
## Note that the survey continuously changed with feedback from colleagues
## Median = 11.2 minutes (good!)
test_rep %>%
  filter(status == "IP Address") %>%
  .$duration_in_seconds %>%
  {as.numeric(.) / 60} %>%
  summary()

# Survey test response =========================================================
## I accidentally went overboard with the number ...
test_rep <- test_rep %>%
  filter(status == "Survey Test") %>%
  ## respondent metadata currently not necessary
  select(
    -start_date, -end_date, -status, -ip_address, -progress,
    -duration_in_seconds, -finished, -recorded_date, -response_id,
    -contains("recipient"), -contains("location"), -distribution_channel,
    -user_language, -q_recaptcha_score, -intro, -external_reference
  ) %>%
  ## delete all text variables for now
  select(-contains("_text")) %>%
  ## I don't understand how Qualtrics text responses are generated
  filter(!is.na(no_context_3_opts_1_1))

# No-context ballot sequences ==================================================
## 3-option case ---------------------------------------------------------------
temp <- test_rep %>%
  unite(
    "no_context_3opt",
    sep = "", no_context_3_opts_1_1:no_context_3_opts_1_3
  ) %>%
  .$no_context_3opt %>%
  table()
round(prop.table(temp) * 100, digits = 1)
##  123  132  213  231  312  321
## 17.8 15.2 15.8 15.2 18.4 17.7

## essentially testing for a fair dice
chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.23

## mmm...
library(pwr)
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
ES.w1(P0, P1) ## effect size (very small: less than 0.1)

## yikes
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.43
pwr.chisq.test(w = ES.w1(P0, P1), N = 2000, df = (length(temp) - 1)) ## 0.77
pwr.chisq.test(w = ES.w1(P0, P1), N = 2500, df = (length(temp) - 1)) ## 0.87

## 4-option case ---------------------------------------------------------------
temp <- test_rep %>%
  unite(
    "no_context_4opt",
    sep = "", no_context_4_opts_1_1:no_context_4_opts_1_4
  ) %>%
  .$no_context_4opt %>%
  table()
round(prop.table(temp) * 100, digits = 1)
## 1234 1243 1324 1342 1423 1432 2134 2143 2314 2341 2413 2431
##  4.5  4.2  4.4  3.6  3.3  5.6  3.7  4.2  2.9  4.6  4.4  3.7
## 3124 3142 3214 3241 3412 3421 4123 4132 4213 4231 4312 4321
##  3.5  3.8  5.1  5.1  4.3  5.9  3.5  3.6  4.0  4.6  3.6  4.0

## ah, interesting.
chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.06
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.92

# Letters ======================================================================
## 3-option case ---------------------------------------------------------------
temp <- test_rep %>%
  unite("letters_3opt", sep = "", letters_3_opts_1:letters_3_opts_3) %>%
  .$letters_3opt %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.83
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.15

## 4-option case ---------------------------------------------------------------
temp <- test_rep %>%
  unite("letters_4opt", sep = "", letters_4_opts_1:letters_4_opts_4) %>%
  .$letters_4opt %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.14
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.88

# Party ID =====================================================================
## 3-option case ---------------------------------------------------------------
temp <- test_rep %>%
  unite("pid_3cand", sep = "", party_id_3_cands_1:party_id_3_cands_3) %>%
  .$pid_3cand %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.007
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.83

## 4-option case ---------------------------------------------------------------
temp <- test_rep %>%
  unite("pid_4cand", sep = "", party_id_4_cands_1:party_id_4_cands_4) %>%
  .$pid_4cand %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.69
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.64
