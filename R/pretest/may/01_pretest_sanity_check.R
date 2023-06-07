source(here::here("R", "utilities.R"))
# df_list <- qualtrics_import("pretest-01-sanitized-numeric.csv")
df_list <- qualtrics_import("pretest-02.csv")
main <- df_list$main
# 3 out of 100, or 3% of rows have recaptcha score less than 0.8.

# Duration =====================================================================
## What is the average duration?
## Median 10.8 and Mean 13.8 in the pre-test
summary(main$duration_in_seconds) / 60

## I find it implausible to have properly answered this survey under 5 min
## but will keep them for now
## 11 out of 97 (11.3%)
sum(main$duration_in_seconds < 60 * 5)
main %>%
  filter(duration_in_seconds < 60 * 5)

# Lucid-generated demographics sanity check ====================================
## Age (allow for 1 year difference)
## 13 out of 97 (13.4%)
main %>%
  filter(abs(as.numeric(age) - as.numeric(age_2)) > 1) %>%
  select(age, age_2)

## Gender
table(main$gender, main$gender_2)

## Race/ethnicity
table(main$race_ces, main$ethnicity)

## Education
table(main$education, main$education_2)

## Partisanship
table(main$pid7, main$political_party)

# Freeform answer based filtering ==============================================
## Some obs that might need to be reconsidered:
## "King bath bath dog dog bath bath bed "
## "Like I said fat"
## "The call to see if I could get back to you soon I love you"
