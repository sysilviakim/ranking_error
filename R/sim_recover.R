# Script Description ===========================================================
# sim_recover.R
# Aim: given observed rankings, attempt to recover true underlying rankings
source(here::here("R", "sim.R"))

# Proof of concept for our bias-correction strategy
## Use data with item order randomization (Assumption 1)
## Bias-correction needs three quantities 
## (alpha, naive estimate, estimate based on 100% random)

## We know this from our simulation (In reality, we must estimate this somehow)
alpha <- 0.5 

## Recovered pref based on the raw data (naive estimate)
est_random <- data_list$p5_rand_0p %>%
  as_tibble() 

## Recovered pref based on the 100% random data 
## (In reality, we must estimate this somehow)
est_naive <- data_list$p6_rand_50p %>%
  as_tibble() 

## Recovered pref based on the sincere ranking (our target)
est_truth <- data_list$p4_rand_100p %>%
  as_tibble() 

p_eps <- table(est_random$obs_rank) / N ## Estimates via 100% pattern rankings
p_obs <- table(est_naive$obs_rank) / N ## Estimates via raw data

## Bias-corrected estimator
p_bc <- (1 / alpha) * (p_obs - (1 - alpha) * p_eps)

## Ground truth
p_QOI <- table(est_truth$obs_rank) / N

# Prepare for the visualization
gg_ob <- data.frame(p_obs)
gg_bc <- data.frame(p_bc)
gg_qoi <- data.frame(p_QOI)
gg_ob$name <- "Naive"
gg_bc$name <- "Bias-corrected"
gg_qoi$name <- "Ground truth"
ggdt <- rbind(gg_ob, gg_bc, gg_qoi) ## Combine all estimates
ggdt ## Check

## Visualize
p <- ggplot(ggdt, aes(x = Var1, y = Freq, fill = name)) +
  geom_bar(stat = "identity", position = "dodge2", alpha = 0.9) +
  scale_fill_manual(values = c("#128ba0", "#a5900d", "gray70")) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
    text = element_text(size = 5)
  )
p
ggsave(here("fig", "proof_of_concept.pdf"), width = 3, height = 3)

# Inverse Probability Weighting on Observed Rankings ===========================
inv <- as_tibble(N / table(obs_half)) %>% rename(obs_rank = obs_half, inv = n)
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

inv_list <- data_list[4:6] %>%
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
