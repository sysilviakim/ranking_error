source(here::here("R", "pretest_import.R"))

# Tate (1993) ==================================================================
tate1993 <- main %>%
  filter(!grepl("9", anc_tate1993) & !grepl("9", app_tate1993))

### Parameters for bootstrapping
N <- nrow(tate1993)
set.seed(12345)
indices <- seq(bootstrap_n) %>%
  map(
    ~ tibble(
      !!as.name(paste0("x", .x)) :=
        sample(seq(N), size = N, replace = TRUE)
    )
  ) %>%
  bind_cols()
assert_that(!identical(sort(indices$x1), sort(indices$x2)))

### Correct answers to anchor question
correct <- ifelse(tate1993$anc_tate1993 == "123", 1, 0)

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
  geom_linerange(aes(x = variable, ymin = low, ymax = up),
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
