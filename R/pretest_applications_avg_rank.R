source(here::here("R", "pretest_import.R"))

# Setup ========================================================================
## Missed the opportunity to see voting, because
## anchor question was incorrectly administered as a single choice question
## later, add voting = "12345"
# sort(c(
#   `1` = "gender", `2` = "city", `3` = "country", `4` = "socio",
#   `5` = "rac", `6` = "poli", `7` = "relig"
# ))

## Truly amazing thing is... that... nobody got the anchor question for
## identity (alphabetical) right?! Why?! It is "2316574"!
## Most freq. pattern "3127456" which is 
## country - gender - city - religion - socioeconomic - racial - political???
## what. am. I. missing. Using 3127456 for now.

root_var <- c(tate1993 = "123", identity = "3127456", nelson1997 = "1234")
prep_list <- root_var %>%
  imap(
    ~ {
      ## For each application, delete partial rankings for both
      ## main and anchor variable. Need respondent to answer both fully.
      dat <- main %>%
        filter(
          !grepl("9", !!as.name(paste0("anc_", .y))) &
            !grepl("9", !!as.name(paste0("app_", .y)))
        )
      N <- nrow(dat)

      ## Indices
      indices <- seq(bootstrap_n) %>%
        map(
          function(y) {
            tibble(
              !!as.name(paste0("x", y)) :=
                sample(seq(N), size = N, replace = TRUE)
            )
          }
        ) %>%
        bind_cols()
      assert_that(!identical(sort(indices$x1), sort(indices$x2)))

      ## Correct answers to the anchor question
      correct <- ifelse(dat[[paste0("anc_", .y)]] == .x, 1, 0)
      return(
        list(
          dat = dat, N = N, indices = indices, correct = correct, answer = .y
        )
      )
    }
  )

# De-Contaminating: avg. rank ==================================================
## Create avg. rank dataset ----------------------------------------------------
ggdat_list <- prep_list %>%
  imap(
    ~ {
      ## Naive estimator 1: heroic assumption of zero non-sincere responses
      est_main_naive_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            avg_rank(
              .x$dat[unlist(.x$indices[, x]), ], paste0("app_", .y)
            )
          }
        ) %>%
        bind_rows()

      J <- nchar(.x$answer)

      ## Unbiased estimation of the proportion of sincere responses (Eq. 9)
      est_p_z1_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            (mean(.x$correct[unlist(.x$indices[, x])]) - (1 / factorial(J))) /
              (1 - (1 / factorial(J)))
          }
        )

      ## Turn into a vector
      vec_pr_z1 <- unlist(est_p_z1_boot)
      vec_pr_z0 <- 1 - vec_pr_z1

      ## Unbiased estimator for the underlying non-sincere dist. in anchor q.
      ## (Eq. 10)
      est_anch_naive_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            avg_rank(
              .x$dat[unlist(.x$indices[, x]), ], paste0("anc_", .y)
            )
          }
        ) %>%
        bind_rows()

      ## De-contaminated ranking data
      est_debiased_boot <-
        (est_main_naive_boot - vec_pr_z0 * est_anch_naive_boot) / vec_pr_z1

      ## Quantiles calculated and into a single dataframe
      ggdat <- list(
        debiased = est_debiased_boot, naive = est_main_naive_boot
      )

      ## Application-specific labels
      if (.y == "tate1993") {
        ggdat <- ggdat %>%
          map(
            ~ .x %>%
              rename(
                Policy = `1st`, Pork = `2nd`, Service = `3rd`
              )
          )
      } else if (.y == "identity") {
        ggdat <- ggdat %>%
          map(
            ~ .x %>%
              rename(
                Gender = `1st`, City = `2nd`, Country = `3rd`,
                SES = `4th`, Race = `5th`, Party = `6th`, Religion = `7th`
              )
          )
      } else if (.y == "nelson1997") {
        ggdat <- ggdat %>%
          map(
            ~ .x %>%
              rename(
                `Freedom of Speech` = `1st`,
                Security = `2nd`,
                Reputation = `3rd`,
                `Anti-racism` = `4th`
              )
          )
      }

      ## Bootstrap
      ggdat <- avg_rank_bootstrap_quantile(ggdat)
      if (.y == "nelson1997") {
        ggdat <- ggdat %>%
          mutate(
            variable = case_when(
              variable == "Anti-racism" ~ "Anti-\nracism",
              variable == "Freedom of Speech" ~ "Freedom\nof Speech",
              TRUE ~ variable
            )
          )
      }

      return(ggdat)
    }
  )

## Visualization ---------------------------------------------------------------
p_list <- ggdat_list %>%
  map(
    ~ ggplot(.x, aes(x = variable, y = mean_val, color = Estimator)) +
      geom_point(
        aes(x = variable, y = mean_val, shape = Estimator),
        size = 2,
        position = position_dodge(width = 0.4)
      ) +
      # Reorder by point estimate
      geom_linerange(
        aes(x = variable, ymin = low, ymax = up),
        lwd = 1, position = position_dodge(width = 0.4)
      ) +
      scale_color_manual(values = c("#E69F00", "#999999")) +
      theme_bw() +
      ylim(-0.1, 4) +
      ylab("Average Rank") + 
      xlab("")
  )

pdf_default(p_list$tate1993) +
  theme(legend.position = "top")
ggsave(
  here("fig/application_tate1993_bootstrapped_avg_rank.pdf"),
  width = 3.5, height = 2.8
)

pdf_default(p_list$nelson1997) +
  theme(legend.position = "top")
ggsave(
  here("fig/application_nelson1997_bootstrapped_avg_rank.pdf"),
  width = 3.5, height = 2.8
)

## doubtful, but trying
pdf_default(p_list$identity) + ylim(1.9, 6) + 
  theme(legend.position = "top")
ggsave(
  here("fig/application_identity_bootstrapped_avg_rank.pdf"),
  width = 5, height = 2.8
)
