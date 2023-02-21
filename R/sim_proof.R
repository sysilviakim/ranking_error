# Script Description ===========================================================
# sim_proof.R
# Created: 2/6/2023
# Aim: simulate our methodology
source(here::here("R", "sim.R"))
set.seed(12345)
s_overlap <- 0.8 ## Another parameter that we could tweak

# Sincerity for Main and Anchor Questions ======================================
## 1 if sincere, 0 if non-sincere
## First, define whether responses will be non-sincere in the anchor question
s_anchor <- rbinom(n = N, size = 1, prob = p_z1)

## Second, in the main question, 80% of the time, 
## non-sincerity will carry over. For the rest, it may be random.
s_main <- c(
  s_anchor[1:floor(s_overlap * N)],
  rbinom(n = floor(s_overlap * N) + 1, size = 1, prob = p_z1)
)
table(s_anchor, s_main)
mean(s_anchor) # must be close to 0.5
mean(s_main) # must be close to 0.5

# 1) Generate the rank-order question of interest ==============================
## Already handled in sim.R; use scenario "skewed_02"

## True obs is defined with respect to the reference choice set (ordered a-b-c)
## Generating *Observed* Ranking (\pi_i) ---------------------------------------
obs_data <- bind_cols(obs_data_list$skewed_02, tibble(s_main = s_main)) %>%
  ## z is a random variable: sincere if 1, non-sincere if 0
  mutate(obs = ifelse(s_main == 1, obs_rank, obs_nonsincere)) %>%
  dplyr::select(obs, starts_with("V"), everything())
head(obs_data)

# 2) Generate the anchor question ==============================================
set.seed(102)

# Creating the correct response in the anchor question
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

## Generate respondents' "stated rankings" -------------------------------------
### *Sincere* respondents
obs_pattern_A <- loop_stated_rank_preference(true_rank_A, random_choices_A)
head(obs_pattern_A)

obs_random_A <- sample(x = perm, size = N, replace = T, prob = pi_epsilon)
obs_random_A <- tibble(obs_nonsincere = obs_random_A)
head(obs_random_A)


## Generating Observed Rank-Order Question -------------------------------------
obs_data_A <- data.frame(
  obs_pattern_A, # sincere responses
  obs_random_A, # non-sincere responses
  random_choices_A, # observed choice set
  s_anchor
) %>%
  # z random variable (1 = sincere)
  # sincere if 1, non-sincere if 0
  mutate(obs = ifelse(s_anchor == 1, obs_rank, obs_nonsincere)) %>%
  dplyr::select(obs, starts_with("V"))

head(obs_data_A) # True obs is defined with respect to the reference set (abc)

### Check two data sets from the main rank-order and anchor questions
head(obs_data)
head(obs_data_A)

# Estimating the QOI using our method ==========================================
## Recovering reference rankings from observed rankings and
## observed item sets (anchor question)
ref_data_A <- recov_ref_ranking(obs_data_A) # This function is in "utilities.R"
head(ref_data_A)
table(ref_data_A)

## Coding z variable
code_z <- ifelse(ref_data_A$ref_ranking == "123", 1, 0)
# 123 means the correct answer

## Estimated proportion of sincere-responses
# est_p_z1 <- mean(code_z)/(1 + 1/factorial(J))
# Adjusting for over-estimation (this was wrong)
A <- mean(code_z) - (1 / factorial(J))
B <- 1 - (1 / factorial(J))
est_p_z1 <- A / B

## This is because P(z=1) = mean(z) - (1/J!)*P(z=1)

## Estimated distribution of non-sincere responses
obs_data_A_aug <- obs_data_A %>%
  mutate(z = code_z) %>%
  filter(z == 1)
est_pi_epsilon <- table(obs_data_A_aug$obs) / sum(code_z)
est_pi_epsilon2 <-
  (table(ref_data_A) / N - (est_p_z1) * c(1, 0, 0, 0, 0, 0)) / (1 - est_p_z1)

## Compare with ground truth
rbind(est_p_z1, p_z1)

## Recovering reference rankings from observed rankings and
## observed item sets (main question)
ref_data <- recov_ref_ranking(obs_data) #
head(ref_data)
table(ref_data)

est_naive <- table(ref_data) / N
est_pi <- (est_naive - (1 - est_p_z1) * est_pi_epsilon2) / est_p_z1

pi <- true_rank %>%
  unite("true_ranking", sep = "") %>%
  table() / N

## Recovering reference rankings from respondents who got the right answer
correct_answer <- obs_data[code_z == 1, ]
correct_data <- recov_ref_ranking(correct_answer)
# only getting data from Rs with the correct answer
head(correct_data)
table(correct_data)
dim(correct_answer)[1] / N
# This will be over 0.5! (bc non-sincere Rs happen to be correct)

est_correct <- table(correct_data) / dim(correct_answer)[1]
rbind(est_naive, est_correct, est_pi, pi)

ggdat <- rbind(est_naive, est_correct, est_pi, pi) %>%
  as_tibble() %>%
  mutate(est = c(
    "Naive Estimator 1", "Naive Estimator 2",
    "Our Method", "Quantity of Interest"
  )) %>%
  pivot_longer(
    cols = !est,
    names_to = "ranking",
    values_to = "proportion"
  )

# Visualization ================================================================
p <- ggplot(ggdat, aes(x = ranking, y = proportion, fill = est)) +
  geom_bar(stat = "identity", position = "dodge2", alpha = 0.7) +
  scale_fill_manual(values = c("gray70", "gray10", "firebrick4", "#a5900d")) +
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.margin = margin(0.2, 0.2, 0, -0.2, "cm")
  )
plot_notitle(pdf_default(p))

ggsave(
  here("fig", "proof_of_concept.pdf"),
  width = 5.5, height = 4
)