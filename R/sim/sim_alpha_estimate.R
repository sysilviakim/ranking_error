# Script Description ===========================================================
# sim_recover.R
# Aim: given observed rankings, attempt to recover true underlying rankings
source(here::here("R", "sim.R"))

# Proof of concept for our bias-correction strategy
## Use data with item order randomization (Assumption 1)
## Bias-correction needs three quantities
## (alpha, naive estimate, estimate based on 100% random)

# POV: Alpha Unknown, 0% Sincere Pattern Known =================================
for (scenario in names(prob_vec_list)) {
  ## Recovered pref based on the 100% random data (0% sincere)
  ## i.e., there's some notion of what pattern exists when 0% sincere
  ## This is what's carrying the analysis

  ## P(pi | z = 0): reference distribution of ranking given non-sincere
  ## For example, in the homogeneous scenario
  ## 123 132 213 231 312 321 
  ## 102 607 276 322 592 101 ---> "non-sincere recipients love certain patterns"
  ## "Observed pattern"
  est_random <- permn_list[[scenario]]$p5_rand_0p
  
  ## Recovered pref based on the sincere ranking (our target)
  ## For example, in the homogeneous scenario
  ## 123  132  213  231  312  321 
  ##   1    1    1    1    1 1995
  est_truth <- permn_list[[scenario]]$p1_fixed_100p
  
  ## 50% mix case ---> Equation (4)
  ## P(pi) = P(pi | z = 1) * P(z = 1) + P(pi | z = 0) * P(z = 0)
  ## Recovered pref based on the raw data (naive estimate)
  ## "Observed pattern"
  est_naive <- permn_list[[scenario]]$p6_rand_50p
  
  p_eps <- table(est_random$obs_rank) / N ## Estimates via 0% sincere (pane E)
  p_obs <- table(est_naive$obs_rank) / N ## Estimates via raw data (pane F)
  p_uniform <- rep(1 / factorial(J), factorial(J))
  names(p_uniform) <- names(p_eps)
  
  ## Estimating "alpha," i.e., P(z = 0) ----------------------------------------
  ## Assume P(pi | z = 1) is simply uniform, given random choice ordering
  ## p_eps * alpha + p_uniform * (1 - alpha) = p_obs
  alpha_true <- 0.5
  
  ## if we knew, we'd expect in our observed data
  ## "We can mix D and E to get F"
  ## "Some difference between the expected F and the real F but trivial diff." 
  expected_obs <- as.numeric(p_eps * alpha_true + p_uniform * (1 - alpha_true))
  summary(expected_obs - as.numeric(p_obs)) ## on avg. zero; small differences
  
  ## p_obs = p_uniform * (1 - alpha_true) + p_eps * alpha_true
  ## alpha_true * (p_eps - p_uniform) = p_obs - p_uniform
  ## Rearranged Equation (4)
  alpha_est <- (as.numeric(p_obs) - as.numeric(p_uniform)) / 
    (as.numeric(p_eps) - as.numeric(p_uniform))
  
  ## because p_eps[4] and p_uniform[4] value very similar,
  ## off significantly
  alpha_est
  summary(alpha_est) ## median 0.50 but mean is 0.67
}

# POV: Alpha Known, 0% Sincere Pattern Unknown =================================

# Inverse Probability Weighting on Observed Rankings ===========================
inv <- as_tibble(N / table(obs_half)) %>% rename(inv = n)
## the actual choices presented
random_choices <- rowid(random_choices)

# Estimate the Quantities of Interest ==========================================
# Transform observed rankings from random-ordered choices into "true" orderings

## obs_rank is the actual realization of the ranking
## the item and rank: what the respondent likes (X)
##                    the order of the items presented to respondent (O)
## For example, 2nd respondent
true_permn[2, ] ## likes a-b-c
random_permn[2, ] ## was presented with c-a-b
obs_pattern[2, ] ## therefore, writes c = 3, a = 1, b = 2

## Now let's recover "true" orderings, or the underlying preference
## i.e., given the c-a-b, can we recover a respondent's ranking?
## How is it different under each situation?

inv_list <- permn_list[[scenario]][4:6] %>%
  map(~ pivot_join(.x, random_choices))
inv_list$p6_rand_50p <- left_join(inv_list$p6_rand_50p, inv, by = "obs_rank")

# Check the "true" ranking data
inv_list %>% map(~ head(.x, 10))

# Distribution of Unique Rankings ==============================================
inv_list <- inv_list %>%
  map(
    ~ .x %>%
      select(-obs_rank) %>%
      pivot_wider(names_from = "rank", values_from = "item") %>%
      unite("true_order", `1`, `2`, `3`, sep = "") %>%
      select(-id)
  )
freq_inv <- inv_list$p6_rand_50p$inv

# Compute the weighted count for the 50% mixture
w_count <- count(x = inv_list$p6_rand_50p, true_order, wt = inv)

# Compare
table(inv_list$p6_rand_50p$true_order) / N # Unweighted
round(w_count$n / sum(w_count$n), d = 4) # Weighted

# Assign inverse probability weights to 50% mixture observations
temp <- obs_half %>% mutate(inv = freq_inv)
head(temp, n = 20) # looks good
prop_vector(temp) # Should look like an identity matrix (with columns reordered)

# Compute the weighted count
weighted_count <- count(x = temp, obs_rank, wt = inv)
weighted_count # Tada! We "corrected" the distortion

# Visualizing What Happens When We Extract "True" Orderings from Data ==========
# with Item Order Randomization
pdf(here("fig", "recover_ranking_sim_zigzag.pdf"), width = 6, height = 4)
p_list <- inv_list %>%
  imap(
    ~ {
      p <- ggplot(.x) +
        geom_bar(
          aes(x = true_order, y = ..count.. / sum(..count..)),
          fill = "deepskyblue3"
        ) +
        xlab("") +
        ylab("") +
        scale_y_continuous(limits = c(0, 0.415), labels = scales::percent)
      return(pdf_default(p))
    }
  )

p_list[[1]] <- p_list[[1]] + ggtitle("A. 100% Attentive")
p_list[[2]] <- p_list[[2]] + ggtitle("B. 0% Attentive")
p_list[[3]] <- p_list[[3]] + ggtitle("C. 50% Attentive")
p_list[[4]] <- ggplot(w_count) +
  geom_col(aes(x = true_order, y = n / sum(n)), fill = "deepskyblue3") +
  xlab("") +
  ylab("")
p_list[[4]] <- pdf_default(p_list[[4]]) +
  ggtitle("D. 50% Attentive (Weighted)") +
  scale_y_continuous(limits = c(0, 0.415), labels = scales::percent)

p <- p_list %>%
  map(~ .x + theme(plot.title = element_text(size = 10))) %>%
  wrap_plots(ncol = 2)
print(p)
dev.off()
