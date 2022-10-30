# Script Description ===========================================================
# sim.R
# Created: 10/22/2022
# Aim: simulate the effects of pattern ranking with various true distributions
source(here::here("R", "utilities.R"))

# Simulation Parameters ========================================================
N <- 2000 # Number of units
J <- 3 # Number of items

# Generate the Sample Space for Simulation =====================================
## Generate the population ranking distribution --------------------------------
set.seed(102)

# If we wanted to use uniform first-choice probability
# prob_vec <- rep(1 / J, J)
# Here, A dominates B and C in the first-choice probability, B and C indifferent
# V1 = first choice, V2 = second choice, V3 = third choice, ...
prob_vec <- c(0.8, 0.1, 0.1)
true_pref <- rpluce(N = N, t = J, prob = prob_vec)
head(true_pref)

# Getting the true ranking for each unit
# Note: if (3, 2, 1), A corresponds to 3, B to 2, and C to 1. We use this later.
# The population has a homogeneous preference.
true_rank <- PLMIX::rank_ord_switch(
  data = true_pref, format_input = "ordering"
) %>%
  as_tibble() %>%
  rename(a = Item_1, b = Item_2, c = Item_3)
true_rank

true_permn <- true_rank %>% unite(order, sep = "")
table(true_permn)

## Generate the ordered choice set in survey question --------------------------
# Assumption 1: Natural order A-B-C, simply observe true permutation
# Assumption 2: Order of items randomized
random_choices <- rpluce(N = N, t = J, prob = rep(1 / J, J))
head(random_choices)

# Check the uniformity: proportions + pearson's chi-squared test
random_permn <- random_choices %>% unite(order, sep = "")
prop_vector(random_permn)
chisq.test(table(random_permn))

## Generate respondents' stated ranked preferences------------------------------
## Under random order of choices

### (D) 100% attentive respondents ---------------------------------------------
obs_pattern <- loop_stated_rank_preference(true_rank, random_choices)
prop_vector(obs_pattern)
chisq.test(table(obs_pattern))

### (B) 0% attentive respondents -----------------------------------------------
# Note: These units only rank according to patterns, not based on preference
# Assuming uniform random patterns, get all elements in the sample space
perm <- combinat::permn(x = 1:J) %>%
  map_chr(~ paste(.x, collapse = "")) %>%
  sort()

# If we wanted uniform patterns
# prob_pt <- rep(1 / length(perm), length(perm))
# A hypothetical zig-zag orient.
prob_pt <- c(0.05, 0.3, 0.15, 0.15, 0.3, 0.05)

obs_random <- sample(x = perm, size = N, replace = T, prob = prob_pt)
obs_random <- tibble(obs_rank = obs_random)

head(obs_random)
prop_vector(obs_random)
chisq.test(table(obs_random))

### (C) 50% attentive respondents (fixed order (a, b, c)) ----------------------
draw1 <- true_permn[1:(N / 2), ] %>% rename(obs_rank = order)
draw2 <- obs_random[1001:N, ]
fixed_half <- rbind(draw1, draw2)

### (F) 50% attentive respondents (item order randomization) -------------------
draw3 <- obs_pattern[1:(N / 2), ]
draw4 <- obs_random[1001:N, ]
obs_half <- rbind(draw3, draw4)
prop_vector(obs_half)

# Compare the Observed Patterns ================================================
pdf(here("fig", "obs_ranking_sim_zigzag.pdf"), width = 7, height = 5)
data_list <- list(
  ## fixed_100p: if 100% attentive, observe true permutation
  p1_fixed_100p = true_permn %>%
    rename(obs_rank = order),
  ## fixed_0p: if 0% attentive, always observe zigzag
  p2_fixed_0p = obs_random,
  ## fixed_50p: mixture
  p3_fixed_50p = fixed_half,
  ## rand_100p: if 100% attentive, this is the observed pattern
  p4_rand_100p = obs_pattern,
  ## rand_0p: if 0% attentive, regardless of randomization, zigzag
  p5_rand_0p = obs_random,
  ## rand_50p: mixture
  p6_rand_50p = obs_half
)

p_list <- data_list %>%
  imap(
    ~ {
      p <- ggplot(.x) +
        geom_bar(
          aes(x = obs_rank, y = ..count.. / sum(..count..)),
          fill = "deepskyblue3"
        ) +
        xlab("") +
        scale_y_continuous(limits = c(0, 0.415), labels = scales::percent)

      ## y-axis label
      if (.y == "p4_rand_100p") {
        p <- p + ylab("Item Order Randomization")
      } else if (.y == "p1_fixed_100p") {
        p <- p + ylab("Fixed Order (a, b, c)")
      } else {
        p <- p + ylab("")
      }
      
      return(pdf_default(p))
    }
  )

p_list[[1]] <- p_list[[1]] + ggtitle("A. 100% Attentive")
p_list[[2]] <- p_list[[2]] + ggtitle("B. 0% Attentive")
p_list[[3]] <- p_list[[3]] + ggtitle("C. 50% Attentive")
p_list[[4]] <- p_list[[4]] + ggtitle("D. 100% Attentive")
p_list[[5]] <- p_list[[5]] + ggtitle("E. 0% Attentive")
p_list[[6]] <- p_list[[6]] + ggtitle("F. 50% Attentive")

p <- p_list %>%
  map(~ .x + theme(plot.title = element_text(size = 10))) %>%
  wrap_plots(ncol = 3)
print(p)
dev.off()

# Inverse Probability Weighting on Observed Rankings ===========================
inv <- as_tibble(N / table(obs_half)) %>% rename(obs_rank = obs_half, inv = n)

temp <- obs_half %>%
  # Silvia, try using "obs_random" as a base dataset for the weighted data
  # (see how weird the result will be!)
  mutate(inv = freq_inv)

head(temp, n = 20) # looks good
prop_vector(temp) # This should look like an identity matrix

# Compute the weighted count
temp_count <- count(x = temp, obs_rank, wt = inv)
temp_count # Tada! We "corrected" the distortion


# Estimate the Quantities of Interest ==========================================

# Transforming observed rankings into "true" orderings
# Create ID for join
obs_pattern$id <- as.numeric(row.names(obs_pattern))
obs_random$id <- as.numeric(row.names(obs_random))
obs_half$id <- as.numeric(row.names(obs_half))
random_choices$id <- as.numeric(row.names(random_choices))

# Check
head(obs_pattern)
head(obs_random)
head(obs_half)
head(random_choices)

## 100% Attentive --------------------------------------------------------------
obs_pref1 <- random_choices %>%
  left_join(obs_pattern) %>%
  pivot_sim()

## 0% Attentive ----------------------------------------------------------------
obs_pref2 <- random_choices %>%
  left_join(obs_random) %>%
  pivot_sim()

## 50% Attentive ---------------------------------------------------------------
obs_pref3 <- random_choices %>%
  left_join(obs_half) %>%
  pivot_sim() %>%
  left_join(inv, by = "obs_rank")

# Check the "true" ranking data
head(obs_pref1)
head(obs_pref2)
head(obs_pref3, n = 20)

# Distribution of Unique Rankings (The Most Case Summary) ======================
## 100% Attentive Respondents --------------------------------------------------
freq_pref1 <- obs_pref1[, c(1, 4, 5)] %>%
  spread(key = "rank", value = "item") %>%
  dplyr::select(-id) %>%
  unite("true_order", sep = "")

## 0% Attentive Respondents ----------------------------------------------------
freq_pref2 <- obs_pref2[, c(1, 4, 5)] %>%
  spread(key = "rank", value = "item") %>%
  dplyr::select(-id) %>%
  unite("true_order", sep = "")

## 50% Attentive Respondents ---------------------------------------------------
freq_pref3 <- obs_pref3[, c(1, 4, 5, 6)] %>%
  spread(key = "rank", value = "item") %>%
  dplyr::select(-id)

head(freq_pref3, n = 20)

### Incorporating weights here
freq_inv <- freq_pref3$inv

freq_temp <- freq_pref3 %>%
  dplyr::select(-inv) %>%
  unite("true_order", sep = "")


head(freq_temp, n = 20)
head(freq_inv)

weighted <- freq_temp %>%
  mutate(inv = freq_inv)

head(weighted, n = 20) # looks good

# Compute the weighted count (am I doing something wrong here????????)
w_count <- count(x = weighted, true_order, wt = inv)

# Compare
table(freq_temp) / N # Unweighted
round(w_count$n / sum(w_count$n), d = 4) # Weighted


# Visualizing What Happens When We Extract "True" Orderings from Data
# with Item Order Randomization
pdf(here("fig/QOI.pdf"), width = 8, height = 7)
par(mfrow = c(2, 2), mar = c(2.5, 2.5, 3, 2), oma = c(0, 4, 2, 0))
barplot(table(freq_pref1) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "A. 100% Attentive")

barplot(table(freq_pref2) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "B. 0% Attentive (Zig-Zag Orientation)")
barplot(table(freq_temp) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "C. 50% Attentive")
barplot(w_count$n / sum(w_count$n),
  main = "",
  col = "deepskyblue3", border = F,
  names.arg = unique(w_count$true_order),
)
title(adj = 0, main = "D. 50% Attentive (Weighted)")
title("*Recovered* Preferences", outer = T, col.main = "deepskyblue4")


dev.off()
