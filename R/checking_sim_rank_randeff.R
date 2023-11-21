source(here::here("R", "utilities.R"))
library(clarify)
library(ranking)

library(devtools) 
devtools::install_github("sysilviakim/ranking")

m1 <- readRDS(file = here("output", "m1.RData")) # ideology (raw data)
m2 <- readRDS(file = here("output", "m2.RData")) # ideology (corrected)
m3 <- readRDS(file = here("output", "m3.RData")) # education (corrected)

# Let's try all possible rankings
tgt_ranking <- c("gender", "race", "party", "religion")
all_rankings <- combinat::permn(tgt_ranking)
out_vec_raw <- list()
out_vec_correct <- list()
out_vec_correct_edu <- list()


for (i in 1:24) {
  out_vec_raw[[i]] <- sim_rank_randeff(
    m = m1,
    permn = all_rankings[[i]],
    random_var = "ideo7",
    range_cont = 1:7,
    seed = 123
  )


  out_vec_correct[[i]] <- sim_rank_randeff(
    m = m2,
    permn = all_rankings[[i]],
    random_var = "ideo7",
    range_cont = 1:7,
    seed = 123
  )

  out_vec_correct_edu[[i]] <- sim_rank_randeff(
    m = m3,
    permn = all_rankings[[i]],
    random_var = "educ",
    range_cont = 1:6,
    seed = 123
  )  
}

out_vec_raw
out_vec_correct


as.list(out_vec_raw) %>%
  bind_rows() %>%
  group_by(ideo7) %>%
  summarize(sum_mean = sum(mean)) %>%
  print()


as.list(out_vec_correct) %>%
  bind_rows() %>%
  group_by(ideo7) %>%
  summarize(sum_mean = sum(mean)) %>%
  print()


as.list(out_vec_correct_edu) %>%
  bind_rows() %>%
  group_by(educ) %>%
  summarize(sum_mean = sum(mean)) %>%
  print()

# Probs sum up to one!

dt_raw <- as.list(out_vec_raw) %>%
  bind_rows() %>%
  mutate(Result = "Raw Data")

dt_correct <- as.list(out_vec_correct) %>%
  bind_rows() %>%
  mutate(Result = "Bias Corrected")

dt_edu <- as.list(out_vec_correct_edu) %>%
  bind_rows() %>%
  mutate(Result = "Bias Corrected")

ggdt <- rbind(dt_raw, dt_correct)


# Visualize the results
ggdt %>%
  ggplot(aes(x = ideo7, y = mean, color = Result)) +
  geom_point(size = 0.3) +
  geom_pointrange(aes(ymin = low, ymax = high), size = 0.3) +
  scale_color_manual(values = c("darkcyan", "darkred")) +
  facet_wrap(~ranking) +
  theme_bw() +
  xlim(1, 7) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ggtitle("Effect of Ideology on Relative Partisanship") -> p

p

ggsave(here::here("placketluce_weight.pdf"),
  width = 12, height = 7
)


# Visualize the results
dt_edu %>%
  ggplot(aes(x = educ, y = mean, color = Result)) +
  geom_point(size = 0.3) +
  geom_pointrange(aes(ymin = low, ymax = high), size = 0.3) +
#  scale_color_manual(values = c("darkcyan", "darkred")) +
  facet_wrap(~ranking) +
  theme_bw() +
  xlim(1, 7) +
  xlab("Education") +
  ylab("Predicted Probability") +
  ggtitle("Effect of Education on Relative Partisanship") -> p

p

ggsave(here::here("placketluce_weight.pdf"),
       width = 12, height = 7
)

# Save the most interesting results seperately
ggdt_sub <- ggdt %>%
  filter(
    Result == "Bias Corrected",
    ranking %in% c(
      "gender_race_religion_party",      
      "gender_race_party_religion",
      "religion_gender_race_party"
    )
  ) %>%
  mutate(ranking = case_when(ranking == "gender_race_religion_party" ~ "gender > race > religion > party",
                             ranking == "gender_race_party_religion" ~ "gender > race > party > religion",
                             ranking == "religion_gender_race_party" ~ "religion > gender > race > party"))

ggdt_sub %>%
  filter(ranking == "gender > race > religion > party") %>%
  ggplot(aes(x = ideo7, y = mean)) +
  geom_point(size = 0.3, color = "darkcyan") +
  geom_pointrange(
    aes(ymin = low, ymax = high),
    size = 0.3, color = "darkcyan"
  ) +
  facet_wrap(~ranking) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  ylim(0, 0.3) +
  xlab("Ideology (1 = most liberal,  7 = most conservative)") +
  ylab("Predicted Probability") -> p_sub

p_sub
ggsave(here::here('fig', "placketluce_ideology_sub1.pdf"),
  width = 4, height = 3
)

ggdt_sub %>%
  filter(ranking == "gender > race > party > religion") %>%
  ggplot(aes(x = ideo7, y = mean)) +
  geom_point(size = 0.3, color = "darkcyan") +
  geom_pointrange(
    aes(ymin = low, ymax = high),
    size = 0.3, color = "darkcyan"
  ) +
  facet_wrap(~ranking) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  ylim(0, 0.3) +
  xlab("Ideology (1 = most liberal,  7 = most conservative)") +
  ylab("Predicted Probability")  -> p_sub


p_sub
ggsave(here::here('fig', "placketluce_ideology_sub2.pdf"),
       width = 4, height = 3
)
ggdt_sub %>%
  filter(ranking == "religion > gender > race > party") %>%
  ggplot(aes(x = ideo7, y = mean)) +
  geom_point(size = 0.3, color = "darkcyan") +
  geom_pointrange(
    aes(ymin = low, ymax = high),
    size = 0.3, color = "darkcyan"
  ) +
  facet_wrap(~ranking) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  ylim(0, 0.3) +
  xlab("Ideology (1 = most liberal,  7 = most conservative)") +
  ylab("Predicted Probability")  -> p_sub


p_sub
ggsave(here::here('fig', "placketluce_ideology_sub3.pdf"),
       width = 4, height = 3
)

