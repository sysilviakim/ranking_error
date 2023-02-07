# Script Description ===========================================================
# sim_proof.R
# Created: 2/6/2023
# Aim: simulate our methodology
source(here::here("R", "utilities.R"))

# Simulation Parameters ========================================================
N <- 2000 # Number of units (parameter to manupulate for Monte Carlo experiments)
J <- 3 # Number of items (parameter to manupulate for Monte Carlo experiments)

p_z1 <- 0.5 # Proportion of sincere responses (parameter to manupulate for Monte Carlo experiments)
s_anchor <- rbinom(n = N, size = 1, prob = p_z1) # 1 if sincere, 0 if non-sincere
s_rankor <- rbinom(n = N, size = 1, prob = p_z1) # 1 if sincere, 0 if non-sincere
mean(s_anchor) # must be close to 0.5
mean(s_rankor) # must be close to 0.5



# Generate the Sample Space for Simulation =====================================
## (1) Generate the rank-order question of interest ----------------------------
set.seed(102)

# If we wanted to use uniform first-choice probability
# prob_vec <- rep(1 / J, J)
# Here, A dominates B and C in the first-choice probability, B and C indifferent
# V1 = first choice, V2 = second choice, V3 = third choice, ...
prob_vec <- c(0.7, 0.2, 0.1)
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


## Generate the ordered choice set in survey question --------------------------
# The observed choice set (the set respondents actually see in the surveys)
random_choices <- rpluce(N = N, t = J, prob = rep(1 / J, J))
head(random_choices)

# Check the uniformity: proportions + pearson's chi-squared test
random_permn <- random_choices %>% unite(order, sep = "")
prop_vector(random_permn)
table(random_permn)
chisq.test(table(random_permn)) # This represents Item Order Randomization


## Generate respondents' "stated rankings" ------------------------------
### *Sincere* respondents ---------------------------------------------
obs_pattern <- loop_stated_rank_preference(true_rank, random_choices)
prop_vector(obs_pattern)
chisq.test(table(obs_pattern)) # Proposition: the dist of observed rankings will be uniform

### *Non-sincere* respondents -------------------------------------------------
# Note: These units only rank according to patterns, not based on preference
# Assuming uniform random patterns, get all elements in the sample space
perm <- combinat::permn(x = 1:J) %>%
  map_chr(~ paste(.x, collapse = "")) %>%
  sort()
perm # permutation space with J items


# A hypothetical zig-zag orientation (parameter to manupulate for Monte Carlo experiments)
pi_epsilon <- c(0.05, 0.3, 0.15, 0.15, 0.3, 0.05)

obs_random <- sample(x = perm, size = N, replace = T, prob = pi_epsilon)
obs_random <- tibble(obs_nonsincere = obs_random)
head(obs_random)
# prop_vector(obs_random)
# table(obs_random)
chisq.test(table(obs_random)) # Proposition: this will never be uniform = p-val must be < 0.001


### Generating Observed Rank-Order Question -----------------------------------------------
obs_data <- data.frame(
  obs_pattern, # sincere responses
  obs_random, # non-sincere responses
  random_choices, # observed choice set
  s_rankor
) %>% # z random variable (1 = sincere)
  mutate(obs = ifelse(s_rankor == 1, obs_rank, obs_nonsincere)) %>% # sincere if 1, non-sincere if 0
  dplyr::select(obs, starts_with("V"))

head(obs_data) # True obs is defined with resepct to the reference set (abc)




## (2) Generate the anchor question --------------------------------------------
set.seed(102)


# Creating the correct response in the anchor question (added on 1/26/2023)
true_rank_A <- tibble(
  a = rep(1, N),
  b = rep(2, N),
  c = rep(3, N)
)
true_rank_A

# Check
# Recovered on 1/26 based on the contextual info
# true_permn <- true_rank_anchor %>% unite(order, sep = "")
# table(true_permn)

## Generate the ordered choice set in survey question --------------------------
# The observed choice set (the set respondents actually see in the surveys)
random_choices_A <- rpluce(N = N, t = J, prob = rep(1 / J, J))
head(random_choices_A)


## Generate respondents' "stated rankings" ------------------------------
### *Sincere* respondents ---------------------------------------------
obs_pattern_A <- loop_stated_rank_preference(true_rank_A, random_choices_A)
head(obs_pattern_A)

obs_random_A <- sample(x = perm, size = N, replace = T, prob = pi_epsilon)
obs_random_A <- tibble(obs_nonsincere = obs_random_A)
head(obs_random_A)


### Generating Observed Rank-Order Question -----------------------------------------------
obs_data_A <- data.frame(
  obs_pattern_A, # sincere responses
  obs_random_A, # non-sincere responses
  random_choices_A, # observed choice set
  s_anchor
) %>% # z random variable (1 = sincere)
  mutate(obs = ifelse(s_anchor == 1, obs_rank, obs_nonsincere)) %>% # sincere if 1, non-sincere if 0
  dplyr::select(obs, starts_with("V"))

head(obs_data_A) # True obs is defined with respect to the reference set (abc)

### Check two data sets from the main rank-order and anchor questions
head(obs_data)
head(obs_data_A)



# Estimating the QOI using our method =====================================
## Recovering reference rankings from observed rankings and observed item sets (anchor question)
ref_data_A <- recov_ref_ranking(obs_data_A) # This function is in "utilities.R"
head(ref_data_A)
table(ref_data_A)

## Coding z variable
code_z <- ifelse(ref_data_A$ref_ranking == "123", 1, 0) # 123 means the correct answer


## Estimated proportion of sincere-responses
est_p_z1 <- mean(code_z)  # This could be always over-estimated (because non-sincere Rs may be right sometimes). What should we do?


## Estimated distribution of non-sincere responses
obs_data_A_aug <- obs_data_A %>%
  mutate(z = code_z) %>%
  filter(z == 1)
est_pi_epsilon <- table(obs_data_A_aug$obs) / sum(code_z)
est_pi_epsilon2 <- (table(ref_data_A) / N - (est_p_z1) * c(1, 0, 0, 0, 0, 0)) / (1 - est_p_z1)

## Compare with ground truth
rbind(est_p_z1, p_z1)


## Recovering reference rankings from observed rankings and observed item sets (main question)
ref_data <- recov_ref_ranking(obs_data) #
head(ref_data)
table(ref_data)

est_pi <- (table(ref_data) / N - (1 - est_p_z1) * est_pi_epsilon2) / est_p_z1


pi <- true_rank %>%
  unite("true_ranking", sep = "") %>%
  table() / N


rbind(est_pi, pi)


# Next step --> We need to think about how to solve the overestimation of 
# est_p_z1: est_p_z1 <- mean(code_z) - (1/factorical(J))*0.5 is the answer
# But we don't know 0.5 in reality
