source(here::here("R", "pretest_zero_weak_context.R"))

# Tate (1993) ==================================================================
## Uniform distribution test ===================================================
### Use order of randomized items to recover observed ranking ------------------
### First, turn text into item numbers from the reference choice set.
main <- text_to_item_position(main) %>%
  unite_ranking() %>%
  ## Lost 10 obs (10.2%)
  filter(!grepl("9", anc_tate1993) & !grepl("9", app_tate1993))

### Recover the "observed" ranking ---------------------------------------------
## based on randomized order presentation
tate1993$anc_tate1993_obs <- tate1993 %>%
  separate(anc_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  recov_ref_ranking(rank_var = "anc_tate1993") %>%
  .$ref

tate1993$app_tate1993_obs <- tate1993 %>%
  separate(app_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  recov_ref_ranking(rank_var = "app_tate1993") %>%
  .$ref

### Create frequency tables ----------------------------------------------------
tab_main <- table(tate1993$app_tate1993_obs)
tab_anch <- table(tate1993$anc_tate1993_obs)

## Note that we've cleaned main and anchor questions' partial rankings
## simultaneously now, not separately
round(prop.table(tab_main) * 100, digits = 1)
#  123  132  213  231  312  321
# 19.3 11.4 25.0 12.5 14.8 17.0
round(prop.table(tab_anch) * 100, digits = 1)
#  123  132  213  231  312  321
# 20.5 14.8  8.0 15.9 20.5 20.5

### Chi-square test and power test ---------------------------------------------
chisq_power(tab_main) ## p-value = 0.2491, ES = 0.275, N = 260+
chisq_power(tab_anch) ## p-value = 0.2606, ES = 0.272, N = 270+

### Visualize distribution -----------------------------------------------------
temp <- table_to_tibble(tab_main)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-tate1993-main.pdf"), width = 4.5, height = 2.8)

temp <- table_to_tibble(tab_anch)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-tate1993-anchor.pdf"), width = 4.5, height = 2.8)

#### Yuki comment: These two graphs offer a great test for Assumptions 1-2
#### It seems to me that one of them (or both) is violated.
#### If non-sincere responses have the same distribution, we should able to
#### observe similar empirical distributions in the two figures
#### One solution is to randomize the order between the anchor and main Qs
#### (Let's try it in our pre-test)


## De-Contaminating ============================================================
### Correct answers to anchor question -----------------------------------------
### So, who got it "right", anyway?
N <- nrow(tate1993)
sum(tate1993$anc_tate1993 == "123") / N ## Yikes, only 58.0%
correct <- ifelse(tate1993$anc_tate1993 == "123", 1, 0)

### Bootstrapping prep ---------------------------------------------------------
set.seed(12345)
bootstrap_n <- 1000

indices <- seq(bootstrap_n) %>%
  map(
    ~ tibble(
      !!as.name(paste0("x", .x)) :=
        sample(seq(N), size = N, replace = TRUE)
    )
  ) %>%
  bind_cols()
assert_that(!identical(sort(indices$x1), sort(indices$x2)))


## New edits:

### Naive and de-contaminated estimators of pi ---------------------------------
### Naive estimator 1: heroic assumption of zero non-sincere responses
#### Function to compute average ranks
avg_rank <- function(var) {
  out <- var %>%
    as_tibble() %>%
    mutate(
      Policy = as.numeric(substr(value, 1, 1)), # from 1st to 1st
      Pork = as.numeric(substr(value, 2, 2)), # from 2nd to 2nd
      Service = as.numeric(substr(value, 3, 3))
    ) %>% # from 3rd to 3rd
    dplyr::select(Policy, Pork, Service) %>%
    summarise_all(mean)

  return(out)
}

est_naive_bootstrap <- seq(bootstrap_n) %>%
  map(~ avg_rank(tate1993$app_tate1993[unlist(indices[, .x])]))

### Cinvert into a data.frame
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
ggsave(here::here("fig/application_tate1993_bootstrapped_avg_rank.pdf"), width = 4, height = 4)
