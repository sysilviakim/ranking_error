source(here::here("R", "utilities.R"))

# Data import ==================================================================
## Qualtrics test responses
test_rep <- read_csv(here(
  "data", "raw", "Non-sincere Ranking_February 21, 2023_03.58.csv"
)) %>%
  clean_names() %>%
  filter(
    start_date != "Start Date" &
      start_date != '{"ImportId":"startDate","timeZone":"America/Denver"}'
  ) %>%
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
  ## also delete all timing variables
  select(-contains("timing"))

# Chi-square Tests and Power Tests =============================================
## No-context ballot sequences =================================================
### 3-option case --------------------------------------------------------------
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
ES.w1(P0, P1) ## effect size 0.078 (very small: less than 0.1)

## yikes
pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.43
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(temp) - 1), power = 0.8) ## 2,132

## well, generally, Cohen's benchmarks of 0.1 (small), 0.3 (medium), 0.5 (large)
## rounded up
pwr.chisq.test(w = 0.1, df = (6 - 1), power = 0.8) ## 1,283
pwr.chisq.test(w = 0.3, df = (6 - 1), power = 0.8) ## 143
pwr.chisq.test(w = 0.5, df = (6 - 1), power = 0.8) ## 52
pwr.chisq.test(w = 0.1, df = (24 - 1), power = 0.8) ## 2,212
pwr.chisq.test(w = 0.3, df = (24 - 1), power = 0.8) ## 246
pwr.chisq.test(w = 0.5, df = (24 - 1), power = 0.8) ## 89

### 4-option case --------------------------------------------------------------
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
ES.w1(P0, P1) ## 0.17

pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.93
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(temp) - 1), power = 0.8) ## 739

## Letters =====================================================================
### 3-option case --------------------------------------------------------------
temp <- test_rep %>%
  unite("letters_3opt", sep = "", letters_3_opts_1:letters_3_opts_3) %>%
  .$letters_3opt %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.83
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
ES.w1(P0, P1) ## 0.04 (tiny!)

pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.15
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(temp) - 1), power = 0.8) ## 6,927

### 4-option case --------------------------------------------------------------
temp <- test_rep %>%
  unite("letters_4opt", sep = "", letters_4_opts_1:letters_4_opts_4) %>%
  .$letters_4opt %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.14
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
ES.w1(P0, P1) ## 0.16

pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.88
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(temp) - 1), power = 0.8) ## 842

## Party ID ====================================================================
### 3-option case --------------------------------------------------------------
temp <- test_rep %>%
  unite("pid_3cand", sep = "", party_id_3_cands_1:party_id_3_cands_3) %>%
  .$pid_3cand %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.007
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
ES.w1(P0, P1) ## 0.12

pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.83
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(temp) - 1), power = 0.8) ## 928

### 4-option case --------------------------------------------------------------
temp <- test_rep %>%
  unite("pid_4cand", sep = "", party_id_4_cands_1:party_id_4_cands_4) %>%
  .$pid_4cand %>%
  table()
round(prop.table(temp) * 100, digits = 1)

chisq.test(temp, p = rep(1 / length(temp), length(temp))) ## p = 0.69
P0 <- rep(1 / length(temp), length(temp))
P1 <- as.numeric(prop.table(temp))
ES.w1(P0, P1) ## 0.13

pwr.chisq.test(w = ES.w1(P0, P1), N = 1000, df = (length(temp) - 1)) ## 0.64
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(temp) - 1), power = 0.8) ## 1,330

# Item Order Randomization and Anchor Question =================================

## Tate (2003) =================================================================
## anc_tate1993_do is the variable representing item order randomization
## in the anchor question
## Reference choice set is {federal, state, municipal},
## and the "correct" answer is also {federal, state, municipal}.
test_rep$anc_tate1993_do <- gsub(
  "Federal government that create policies that affect people's lives at the federal level",
  "1",
  gsub(
    "State government that create policies that affect people's lives at the state level",
    "2",
    gsub(
      "Municipal government that create policies that affect people's lives at the city level",
      "3",
      gsub("\\|", "", test_rep$anc_tate1993_do)
    )
  )
)
prop.table(table(test_rep$anc_tate1993_do))

test_rep <- test_rep %>%
  separate(anc_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  unite(obs, anc_tate1993_1:anc_tate1993_3, sep = "") %>%
  select(obs, V1, V2, V3, contains("tate"), everything())

