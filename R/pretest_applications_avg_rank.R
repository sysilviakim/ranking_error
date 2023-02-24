source(here::here("R", "pretest_import.R"))

# Tate (1993) ==================================================================
tate1993 <- main %>%
  filter(!grepl("9", anc_tate1993) & !grepl("9", app_tate1993))

### Parameters for bootsrapping
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

## De-Contaminating ============================================================
### Naive and de-contaminated estimators of pi ---------------------------------
### Naive estimator 1: heroic assumption of zero non-sincere responses
#### Function to compute average ranks

est_naive_bootstrap <- seq(bootstrap_n) %>%
  map(~ avg_rank(tate1993$app_tate1993[unlist(indices[, .x])]))

### Convert into a data.frame
tbl_pi <- do.call(rbind.data.frame, est_naive_bootstrap)

## Unbiased estimation of the proportion of sincere responses (Eq. 9)
est_p_z1_bootstrap <- seq(bootstrap_n) %>%
  map(
    ~ (mean(correct[unlist(indices[, .x])]) - (1 / factorial(3))) /
      (1 - (1 / factorial(3)))
  )

### Turn into a vector
vec_pr_z1 <- do.call(rbind.data.frame, est_p_z1_bootstrap)
vec_pr_z1 <- pull(vec_pr_z1) # Make it to a vector
vec_pr_z0 <- 1 - vec_pr_z1

## Unbiased estimator for the underlying non-sincere dist. in anchor q. (Eq. 10)
est_pi_anc_naive_bootstrap <- seq(bootstrap_n) %>%
  map(~ avg_rank(tate1993$anc_tate1993[unlist(indices[, .x])]))

tbl_pi_n_anc <- do.call(rbind.data.frame, est_pi_anc_naive_bootstrap)

## De-contaminated ranking data
est_pi_bootstrap <- (tbl_pi - vec_pr_z0 * tbl_pi_n_anc) / vec_pr_z1

gg_est <- est_pi_bootstrap %>%
  gather("variable", "value") %>%
  group_by(variable) %>%
  summarize(
    mean_val = mean(value),
    low = quantile(value, probs = 0.025),
    up = quantile(value, probs = 0.975)
  ) %>%
  mutate(Estimator = "De-biased")

gg_raw <- tbl_pi %>%
  gather("variable", "value") %>%
  group_by(variable) %>%
  summarize(
    mean_val = mean(value),
    low = quantile(value, probs = 0.025),
    up = quantile(value, probs = 0.975)
  ) %>%
  mutate(Estimator = "Naive")

### Combine data ---------------------------------------------------------------
ggdat <- rbind(gg_est, gg_raw)

### Visualization --------------------------------------------------------------
p <- ggplot(ggdat, aes(x = variable, y = mean_val, color = Estimator)) +
  geom_point(aes(x = variable, y = mean_val),
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
  xlab("Tate (1993): Representation") +
  theme(legend.position = "top")

p
ggsave(
  here("fig/application_tate1993_bootstrapped_avg_rank.pdf"),
  width = 4, height = 4
)
