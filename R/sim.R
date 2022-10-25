# Script description ===========================================================
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
prob_vec <- rep(1 / J, J) # Uniform first-choice probability (can be MODIFIED)
prob_vec <- c(0.8, 0.1, 0.1) # A dominates B and C in the first-choice prob.

print(prob_vec)

true_pref <- rpluce(N = N, t = J, prob = prob_vec) %>%
  as_tibble()

head(true_pref) # Check

# Check the uniformity

# Getting the true ranking for each unit
true_rank <- PLMIX::rank_ord_switch(
  data = true_pref,
  format_input = "ordering"
) %>%
  as_tibble()

true_rank # Item_1 = A, Item _2 = B, Item_3 = C
# Note: if (3,2,1) this means A corresponds 3, B 2, and C 1. We use this later.

check <- true_rank %>% unite(order, sep = "")
table(check)

## Generate the ordered choice set in survey question --------------------------
randomize <- rep(1 / J, J) # Assumption 1: Order randomization
choice <- rpluce(N = N, t = J, prob = randomize) %>%
  as_tibble()
head(choice)

# Check the uniformity
check2 <- choice %>% unite(order, sep = "")
table(check2)

## Generate respondents' stated ranked preferences------------------------------

# (1) 100% attentive respondents ===============================================

# Storage for observed patterns
obs_pattern <- data.frame(
  pattern1 = integer(),
  pattern2 = integer(),
  pattern3 = integer()
)

## Loop ------------------------------------------------------------------------
for (i in 1:dim(true_rank)[1]) { # For each i-th unit in true_rank

  # Cast numbers to alphabets
  vec_pref <- true_rank[i, ] %>%
    reshape2::melt() %>%
    mutate(variable = case_when(
      variable == "Item_1" ~ "a",
      variable == "Item_2" ~ "b",
      variable == "Item_3" ~ "c"
    ))
  vec_pref # Check

  # Alphabet unit i sees in each position
  position1 <- choice[i, 1] %>% pull()
  position2 <- choice[i, 2] %>% pull()
  position3 <- choice[i, 3] %>% pull()

  # Assign a value (rank) for each position given the alphabet unit i sees there
  pattern1 <- vec_pref$value[vec_pref$variable == position1]
  pattern2 <- vec_pref$value[vec_pref$variable == position2]
  pattern3 <- vec_pref$value[vec_pref$variable == position3]

  # Combine the result
  comb <- data.frame(
    pattern1 = pattern1,
    pattern2 = pattern2,
    pattern3 = pattern3
  )

  # Stack in the storage
  obs_pattern <- rbind(obs_pattern, comb)
} # End of i loop

obs_pattern <- obs_pattern %>% unite(obs_rank, sep = "")
table(obs_pattern)

# (2) 0% attentive respondents =================================================
# Note: These units only rank according to patterns, not based on preference
# Assuming uniform random patterns
perm <- combinat::permn(x = 1:J) # Get all elements in the sample space
perm <- as.data.frame(do.call(cbind, perm)) %>%
  t() %>%
  as_tibble() %>%
  unite(rank, sep = "") %>%
  pull() %>%
  sort()
perm

n_perm <- length(perm) # Size of the permutation space
prob_pt <- rep(1 / n_perm, n_perm) # Uniform patterns (can be MODIFIED later)
prob_pt <- c(0.05, 0.3, 0.15, 0.15, 0.3, 0.05) # Zig-zag orientation

obs_random <- sample(x = perm, size = N, replace = T, prob = prob_pt)
obs_random <- data.frame(obs_rank = obs_random)

head(obs_random)
table(obs_random)

# (3) 50% attentive respondents (Fixed Order (a,b,c)) --------------------------
draw1 <- check[1:(N / 2), ] %>% rename(obs_rank = order)
draw2 <- tibble(obs_rank = obs_random[1001:N, ])

check_half <- rbind(draw1, draw2)

 # (3) 50% attentive respondents (item order randomization) ---------------------

draw3 <- tibble(obs_rank = obs_pattern[1:(N / 2), ])
draw4 <- tibble(obs_rank = obs_random[1001:N, ])

obs_half <- rbind(draw3, draw4)
table(obs_half)

# (2) Compare the observed patterns ============================================

pdf(here("fig/ObsRanking.pdf"), width = 9, height = 6)
par(mfrow = c(2, 3), mar = c(2.5, 2.5, 3, 2), oma = c(0, 4, 2, 0))
barplot(table(check) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "A. 100% Attentive")
mtext("Fixed Order (a,b,c)",
  side = 2, line = 4, cex = 1.2, font = 2, col = rgb(0.1, 0.3, 0.5, 0.5)
)
barplot(table(obs_random) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "B. 0% Attentive (Zig-Zag Orientation)")
barplot(table(check_half) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "C. 50% Attentive")


barplot(table(obs_pattern) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "D. 100% Attentive")
mtext("Item Order Randomization",
  side = 2, line = 4, cex = 1.2, font = 2, col = rgb(0.1, 0.3, 0.5, 0.5)
)
barplot(table(obs_random) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "E. 0% Attentive (Zig-Zag Orientation)")
barplot(table(obs_half) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "F. 50% Attentive", )
title("*Observed* Ranking Patterns", outer = T, col.main = "deepskyblue4")

dev.off()

# Estimate the Quantities of Interest ==========================================

# Transforming observed rankings into "true" orderings
# Create ID for join
obs_pattern$id <- as.numeric(row.names(obs_pattern))
obs_random$id <- as.numeric(row.names(obs_random))
obs_half$id <- as.numeric(row.names(obs_half))
choice$id <- as.numeric(row.names(choice))

# Check
head(obs_pattern)
head(obs_random)
head(obs_half)
head(choice)

## 100% Attentive --------------------------------------------------------------
obs_pref1 <- choice %>%
  left_join(obs_pattern) %>% # Join two datasets
  # Turn into a long format
  gather("V1", "V2", "V3", key = "position", value = "item") %>%
  mutate(rank = case_when(
    position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
    position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
    position == "V3" ~ str_sub(obs_rank, 3, 3)
  )) %>% # Third value
  arrange(id, rank)

## 0% Attentive ----------------------------------------------------------------
obs_pref2 <- choice %>%
  left_join(obs_random) %>% # Join two datasets
  # Turn into a long format
  gather("V1", "V2", "V3", key = "position", value = "item") %>%
  mutate(rank = case_when(
    position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
    position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
    position == "V3" ~ str_sub(obs_rank, 3, 3)
  )) %>% # Third value
  arrange(id, rank)

## 50% Attentive ---------------------------------------------------------------
obs_pref3 <- choice %>%
  left_join(obs_half) %>% # Join two datasets
  # Turn into a long format
  gather("V1", "V2", "V3", key = "position", value = "item") %>%
  mutate(rank = case_when(
    position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
    position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
    position == "V3" ~ str_sub(obs_rank, 3, 3)
  )) %>% # Third value
  arrange(id, rank)

# Check the "true" ranking data
head(obs_pref1)
head(obs_pref2)
head(obs_pref3)

# 2.1: Distribution of unique rankings (the most cease summary) ================
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
freq_pref3 <- obs_pref3[, c(1, 4, 5)] %>%
  spread(key = "rank", value = "item") %>%
  dplyr::select(-id) %>%
  unite("true_order", sep = "")

# Visualizing What Happens When We Extract "True" Orderings from Data
# with Item Order Randomization
pdf(here("fig/QOI.pdf"), width = 8, height = 3)
par(mfrow = c(1, 3), mar = c(2.5, 2.5, 3, 2), oma = c(0, 4, 2, 0))
barplot(table(freq_pref1) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "A. 100% Attentive")
mtext("Item Order Randomization",
  side = 2, line = 4, cex = 1.2, font = 2, col = rgb(0.1, 0.3, 0.5, 0.5)
)
barplot(table(freq_pref2) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "B. 0% Attentive (Zig-Zag Orientation)")
barplot(table(freq_pref3) / N,
  main = "",
  col = "deepskyblue3", border = F
)
title(adj = 0, main = "C. 50% Attentive")
title("*Recovered* Preferences", outer = T, col.main = "deepskyblue4")
dev.off()
