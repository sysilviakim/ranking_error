# Script Description ===========================================================
# sim_proof.R
# Created: 2/6/2023
# Aim: simulate our methodology
source(here::here("R", "sim.R"))
set.seed(12345)
z_overlap <- 0.8 ## Another parameter that we could tweak

# Sincerity for Main and Anchor Questions ======================================
## 1 if sincere, 0 if non-sincere
## First, define whether responses will be non-sincere in the anchor question
z_anchor <- rbinom(n = N, size = 1, prob = p_z1)

## Second, in the main question, 80% of the time,
## non-sincerity will carry over. For the rest, it may be random.
z_main <- c(
  z_anchor[1:floor(z_overlap * N)],
  rbinom(n = floor((1 - z_overlap) * N) + 1, size = 1, prob = p_z1)
)
table(z_anchor, z_main)
mean(z_anchor) # must be close to 0.5
mean(z_main) # must be close to 0.5

# 1) The Main Rank-Order Question and its Observed Rankings ====================
## Already handled in sim.R; use scenario "skewed_02"

## True obs is defined with respect to the reference choice set (ordered a-b-c)
## Generating *Observed* Ranking (\pi_i)
obs_data_main <- bind_cols(obs_data_list$skewed_02, tibble(z_main = z_main)) %>%
  ## z is a random variable: 1 if sincere, 0 if non-sincere
  mutate(obs = ifelse(z_main == 1, obs_rank, obs_nonsincere)) %>%
  dplyr::select(obs, starts_with("V"), everything())
head(obs_data_main)

# 2) The Anchor Question and its Observed Rankings =============================
obs_data_A <- bind_cols(obs_data_list$anchor, tibble(z_anchor = z_anchor)) %>%
  mutate(obs = ifelse(z_anchor == 1, obs_rank, obs_nonsincere)) %>%
  dplyr::select(obs, starts_with("V"), everything())
head(obs_data_A)

## Check two data sets from the main rank-order and anchor questions
head(obs_data_main)
head(obs_data_A)

# Estimating the QOI via De-Contamination ======================================
## Recovering reference rankings from observed rankings and
## observed item sets (anchor question)
ref_data_A <- recov_ref_ranking(obs_data_A)
head(ref_data_A)
table(ref_data_A)

## Coding random variable c, i.e., whether respondent offers the correct answer
## 1 if correct, 0 if not
correct <- ifelse(ref_data_A$ref_ranking == "123", 1, 0)
# 123 means the correct answer

## Unbiased estimation of the proportion of sincere-responses (Eq. pz)
## est_p_z1 <- mean(correct) / (1 + 1 / factorial(J))
## Adjusting for over-estimation (this was wrong) <- ??
est_p_z1 <- (mean(correct) - (1 / factorial(J))) / (1 - (1 / factorial(J)))
## This is because P(z=1) = mean(z) - (1/J!)*P(z=1) <- ??

## Estimated distribution of non-sincere responses
obs_data_A_aug <- obs_data_A %>%
  mutate(c = correct) %>%
  filter(c == 1)
## est_pi_epsilon <- table(obs_data_A_aug$obs) / sum(correct) <- ??
est_pi_nonsincere <-
  (table(ref_data_A) / N - (est_p_z1) * c(1, 0, 0, 0, 0, 0)) / (1 - est_p_z1)

## Compare with ground truth
rbind(est_p_z1, p_z1)

## Recovering reference rankings from observed rankings and
## observed item sets (main question)
ref_data <- recov_ref_ranking(obs_data_main)
head(ref_data)
table(ref_data)

## Naive estimator 1: heroic assumption of zero non-sincere responses
est_naive <- table(ref_data) / N
est_pi <- (est_naive - (1 - est_p_z1) * est_pi_nonsincere) / est_p_z1

## Extract the true permn distribution, which was saved!
pi <- obs_data_list$skewed_02$true_permn %>%
  table() / N

## Recovering reference rankings from respondents who got the right answer
correct_answer <- obs_data_main[correct == 1, ]
correct_data <- recov_ref_ranking(correct_answer)

## only getting data from Rs with the correct answer
head(correct_data)
table(correct_data)
dim(correct_answer)[1] / N
## This will be >= 0.5, because some non-sincere Rs happen to be correct!

## Naive estimator 2: simply use Rs with correct responses to anchor question
est_correct <- table(correct_data) / dim(correct_answer)[1]
rbind(est_naive, est_correct, est_pi, pi)

ggdat <- rbind(est_naive, est_correct, est_pi, pi) %>%
  as_tibble() %>%
  mutate(
    est = c(
      "Naive Estimator 1", "Naive Estimator 2",
      "Our Method", "Quantity of Interest"
    )
  ) %>%
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

# Bootstrapping ================================================================


