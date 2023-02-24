source(here::here("R", "pretest_import.R"))

# Setup ========================================================================
## Missed the opportunity to see voting, because
## anchor question was incorrectly administered as a single choice question
## later, add voting = "12345"
# sort(c(
#   `1` = "gender", `2` = "city", `3` = "country", `4` = "socio",
#   `5` = "rac", `6` = "poli", `7` = "relig"
# ))
root_var <- c(tate1993 = "123", identity = "2316574", nelson1997 = "1234")
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
          function(y) tibble(
            !!as.name(paste0("x", y)) :=
              sample(seq(N), size = N, replace = TRUE)
          )
        ) %>%
        bind_cols()
      assert_that(!identical(sort(indices$x1), sort(indices$x2)))
      
      ## Correct answers to the anchor question
      correct <- ifelse(dat[[paste0("anc_", .y)]] == .x, 1, 0)
      return(list(dat = dat, N = N, indices = indices, correct = correct))
    }
  )

## De-Contaminating: avg. rank =================================================
### Naive estimator 1: heroic assumption of zero non-sincere responses
est_main_naive_boot <- seq(bootstrap_n) %>%
  map(~ avg_rank(tate1993[unlist(indices[, .x]), ], "app_tate1993")) %>%
  bind_rows()

### Unbiased estimation of the proportion of sincere responses (Eq. 9)
est_p_z1_boot <- seq(bootstrap_n) %>%
  map(
    ~ (mean(correct[unlist(indices[, .x])]) - (1 / factorial(3))) /
      (1 - (1 / factorial(3)))
  )

### Turn into a vector
vec_pr_z1 <- unlist(est_p_z1_boot)
vec_pr_z0 <- 1 - vec_pr_z1

## Unbiased estimator for the underlying non-sincere dist. in anchor q. (Eq. 10)
est_anch_naive_boot <- seq(bootstrap_n) %>%
  map(~ avg_rank(tate1993[unlist(indices[, .x]), ], "anc_tate1993")) %>%
  bind_rows()

## De-contaminated ranking data
est_debiased_boot <-
  (est_main_naive_boot - vec_pr_z0 * est_anch_naive_boot) / vec_pr_z1

## Quantiles calculated and into a single dataframe
ggdat <- list(debiased = est_debiased_boot, naive = est_main_naive_boot) %>%
  map(
    ## Application-specific labels
    ~ .x %>% rename(Policy = `1st`, Pork = `2nd`, Service = `3rd`)
  ) %>%
  avg_rank_bootstrap_quantile()

### Visualization --------------------------------------------------------------
p <- ggplot(ggdat, aes(x = variable, y = mean_val, color = Estimator)) +
  geom_point(aes(x = variable, y = mean_val, shape = Estimator),
    size = 2,
    position = position_dodge(width = 0.4)
  ) + # Reorder by point estimate
  geom_linerange(
    aes(x = variable, ymin = low, ymax = up),
    lwd = 1,
    position = position_dodge(width = 0.4)
  ) +
  scale_color_manual(values = c("#E69F00", "#999999")) +
  theme_bw() +
  ylim(-0.2, 3.5) +
  ylab("Average Rank") +
  xlab("Tate (1993): Representation")

pdf_default(p) +
  theme(legend.position = "top")
ggsave(
  here("fig/application_tate1993_bootstrapped_avg_rank.pdf"),
  width = 4, height = 4
)
