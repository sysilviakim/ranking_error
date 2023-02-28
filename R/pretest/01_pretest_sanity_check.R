source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-01-sanitized-numeric.csv")
main <- df_list$main

# Duration =====================================================================
## What is the average duration?
## Median 10.0 and Mean 12.9 in the pre-test
summary(main$duration_in_seconds) / 60

## I find it implausible to have properly answered this survey under 5 min
## but will keep them for now
## 12 out of 102 (11.8%)
sum(main$duration_in_seconds < 60 * 5)
main %>%
  filter(duration_in_seconds < 60 * 5)

# Lucid-generated demographics sanity check ====================================
## Age
main %>%
  filter(age != age_2) %>%
  select(age, age_2)

## Some very odd observations

# Freeform answer based filtering ==============================================

