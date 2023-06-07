source(here::here("R", "utilities.R"))

# Export conditions
# --- Download all fields
# --- Use numeric values
# --- Recode seen but unanswered questions as -99
# --- Export viewing order data for randomized surveys

df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main
# 3 out of 101, or 3% of rows have recaptcha score less than 0.8 and dropped.

# Duration =====================================================================
## What is the average duration?
## Median 12.39 and mean 19.58 in the pre-test
## Qualtrics estimated runtime is 19.3 minutes
## One respondent took 2 hours, another 7.4 hours
summary(main$duration_in_minutes)

## I find it implausible to have properly answered this survey under 5 min
## 5 out of 98 (5.1%)
sum(main$duration_in_minutes < 5)
main %>%
  filter(duration_in_minutes < 5)

# Lucid-generated demographics sanity check ====================================
## Age (allow for 1 year difference)
## 4 out of 98 (4.1%)
main %>%
  filter(abs(as.numeric(age) - as.numeric(age_2)) > 1) %>%
  select(age, age_2)

## Gender
## 1 mismatch
table(main$gender, main$gender_2)

## Race/ethnicity
table(main$race, main$ethnicity)

## Education
table(main$education, main$education_2)

## Partisanship
table(main$pid3, main$political_party)

# Freeform answer based filtering ==============================================
## Much better this time
