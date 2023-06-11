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

# Proof of concept for anchors==================================================
## What is the duraion for each ranking question?
## We want anchors to have similar durations to main questions.

time <- df_list$timing %>%
  dplyr::select(contains(c("identity", "polar", "tate", "systems"))) %>%
  dplyr::select(contains(c("submit"))) %>%
  dplyr::select(order(colnames(.))) %>%
  mutate_if(is.character, as.numeric)

col <- c("#b0015a", "#128ba0", "#a5900d", "gray")

## Visualize and save

pdf(here::here("fig", "pretest03-check_duration.pdf"), width=6, height=6)
par(mfrow=(c(2,2)))

plot(density(time$time_anc_tate_1993_page_submit), 
     type="n", main="Representation (J=3)\n48% geometric", 
     xlab="Completion time in seconds (N=101)", xlim=c(0,200))
lines(density(time$time_app_tate_1993_page_submit), col=col[1])
lines(density(time$time_anc_tate_1993_page_submit), col=col[1], lty=2)
legend(x=100, y=0.018, legend=c("Main", "Anchor"),
       col=col[1], lty=1:2, cex=1, box.lty=0)

plot(density(time$timing_app_e_systems_page_submit), 
     type="n", main="Electoral Systems (J=7)\n84.7% geometric", 
     xlab="Completion time in seconds (N=101)", xlim=c(0,200))
lines(density(time$time_anc_e_systems_page_submit), col=col[2])
lines(density(time$timing_app_e_systems_page_submit), col=col[2], lty=2)

plot(density(time$time_app_identity_page_submit), 
     type="n", main="Identity (J=7)\n72.4% geometric", 
     xlab="Completion time in seconds (N=101)", xlim=c(0,200))
lines(density(time$time_app_identity_page_submit), col=col[3])
lines(density(time$time_anc_identity_page_submit), col=col[3], lty=2)

plot(density(time$time_app_aff_polar_page_submit), 
     type="n", main="Polarization (J=8)\n46.0% geometric", 
     xlab="Completion time in seconds (N=101)", xlim=c(0,200))
lines(density(time$time_app_aff_polar_page_submit), col=col[4])
lines(density(time$time_anc_polar_page_submit), col=col[4], lty=2)

dev.off()


