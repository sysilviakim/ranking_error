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

# Geometric patterning rates ===================================================
main %>%
  select(matches("^ns_")) %>%
  map(~ round(sum(.x == 1) / nrow(main) * 100, digits = 1))

## Tate 1993: 48.0%
## Electoral systems: 84.7%
## Identity: 72.4%
## Polarization: 46.0%

## Yikes.
sort(table(main$anc_e_systems), decreasing = TRUE)
sort(table(main$anc_identity), decreasing = TRUE)

# Compare duration between main and anchor =====================================
## What is the duration for each ranking question?
## We want anchors to have similar durations to main questions.

time <- df_list$timing %>%
  ## Filtered for reasonable captcha scores
  filter(response_id %in% main$response_id) %>%
  select(contains(c("identity", "polar", "tate", "systems"))) %>%
  select(contains(c("submit"))) %>%
  select(order(colnames(.))) %>%
  mutate(across(where(is.character), as.numeric))

## Visualize and save
## ggplot version (SK) ---------------------------------------------------------
## Fix legend + make title also automatic
duration_plot_list <- names(root_var) %>%
  set_names(., .) %>%
  map(
    ~ list(
      anc = paste0("time_anc_", .x, "_page_submit"),
      app = paste0("time_app_", .x, "_page_submit"),
      col = case_when(
        .x == "tate" ~ "#b0015a",
        .x == "e_systems" ~ "#128ba0",
        .x == "identity" ~ "#a5900d",
        TRUE ~ "gray"
      ),
      title = case_when(
        .x == "tate" ~ "Representation (J = 3)\n48.0% Geometric",
        .x == "e_systems" ~ "Electoral Systems (J = 7)\n84.7% Geometric",
        .x == "identity" ~ "Identity (J = 7)\n72.4% Geometric",
        TRUE ~ "Polarization (J = 8)\n45.9% Geometric"
      )
    )
  ) %>%
  imap(
    ~ ggplot(time) +
      geom_density(
        aes(x = !!as.name(.x$app)),
        linetype = "solid", color = .x$col
      ) +
      geom_density(
        aes(x = !!as.name(.x$anc)),
        linetype = "dashed", color = .x$col
      ) +
      xlab(paste0("Completion time in seconds (N = ", nrow(main), ")")) +
      ylab("Density") +
      ggtitle(.x$title) +
      scale_x_continuous(limits = c(0, 60 * 5)) +
      scale_y_continuous(limits = c(0, 0.03))
  ) %>%
  imap(
    ~ {
      if (.y == "tate") {
        return(plot_nolegend(pdf_default(.x)))
      } else {
        return(pdf_default(.x) + theme(legend.position = c(200, 0.025)))
      }
    }
  )

# Duration Between Main and Anchor Question,\nTruncated at 5 Minutes
ggarrange(plotlist = duration_plot_list)

## base R version (YA) ---------------------------------------------------------
col <- c("#b0015a", "#128ba0", "#a5900d", "gray")

pdf(
  here("fig", "pretest", "pretest03-check_duration.pdf"),
  width = 6, height = 6
)
par(mfrow = (c(2, 2)))

plot(
  density(time$time_anc_tate_page_submit),
  type = "n", main = "Representation (J = 3)\n48% Geometric",
  xlab = "Completion time in seconds (N = 98)", xlim = c(0, 200)
)
lines(density(time$time_app_tate_page_submit), col = col[1])
lines(density(time$time_anc_tate_page_submit), col = col[1], lty = 2)
legend(
  x = 100, y = 0.018, legend = c("Main", "Anchor"),
  col = col[1], lty = 1:2, cex = 1, box.lty = 0
)

plot(
  density(time$time_app_e_systems_page_submit),
  type = "n", main = "Electoral Systems (J = 7)\n84.7% Geometric",
  xlab = "Completion time in seconds (N = 98)", xlim = c(0, 200)
)
lines(density(time$time_anc_e_systems_page_submit), col = col[2])
lines(density(time$time_app_e_systems_page_submit), col = col[2], lty = 2)

plot(
  density(time$time_app_identity_page_submit),
  type = "n", main = "Identity (J = 7)\n72.4% Geometric",
  xlab = "Completion time in seconds (N = 98)", xlim = c(0, 200)
)
lines(density(time$time_app_identity_page_submit), col = col[3])
lines(density(time$time_anc_identity_page_submit), col = col[3], lty = 2)

plot(density(time$time_app_polar_page_submit),
  type = "n", main = "Polarization (J = 8)\n45.9% Geometric",
  xlab = "Completion time in seconds (N = 98)", xlim = c(0, 200)
)
lines(density(time$time_app_polar_page_submit), col = col[4])
lines(density(time$time_anc_polar_page_submit), col = col[4], lty = 2)

dev.off()
