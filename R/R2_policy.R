source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Grab main data
main <- df_list$main

# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)
# Grab rankings and weights
imp_w <- read_csv(here::here("data/tidy", "temp_weight.csv")) %>%
  mutate(
    app_identity = as.character(ranking),
    bias_weight = weight
  ) %>%
  dplyr::select(app_identity, est.x.adj)


# Downsize data
dt <- main %>%
  mutate(id = 1:nrow(main)) %>%
  rename(
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race_ethnicity = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  rename(bias_weight = est.x.adj) %>%
  dplyr::select(
    party, religion, gender, race_ethnicity, id, ideo7, bias_weight
  ) %>%
  filter(!is.na(ideo7))

# "sample" from correct PMF
set.seed(142)
rank_sample <- NA
for (i in 1:1081) {
  rank_sample[i] <-
    imp_w$app_identity[rmultinom(1, size = 1, prob = imp_w$est.x.adj) == 1]
}

rank_sample <- rank_sample %>%
  tibble() %>%
  mutate(
    party = as.integer(substr(., 1, 1)),
    religion = as.integer(substr(., 2, 2)),
    gender = as.integer(substr(., 3, 3)),
    race_ethnicity = as.integer(substr(., 4, 4))
  ) %>%
  dplyr::select(-.)


# Run Unfolding ================================================================
# (there is no way to incorporate weights..., so I improvised)
library(smacof)

set.seed(142)
out <- unfolding(dt[, 1:4], type = "ordinal", circle = "column")
out_w <- unfolding(rank_sample, type = "ordinal", circle = "column")



# Predict policy positions =====================================================
conf_persons <- as.data.frame(out_w$conf.row) 
main <- main %>%
  filter(!is.na(ideo7)) %>%
  mutate(bn_police = ifelse(police == 1, 1, 0),
         bn_reform = ifelse(reform == 1, 1, 0),
         bn_gun = ifelse(gun == 1 , 1, 0),
         bn_abortion = ifelse(abortion == 1, 1, 0),
         bn_environment = ifelse(environment == 1, 1, 0),
         rank_party = app_identity_1,
         rank_religion = app_identity_2,
         rank_gender = app_identity_3,
         rank_race_ethnicity = app_identity_4,
         ordering = case_when(app_identity == 1234 ~ "Party-Religion-Gender-Race",
                              app_identity == 1243 ~ "Party-Religion-Race-Gender",
                              app_identity == 1324 ~ "Party-Gender-Religion-Race",
                              app_identity == 1342 ~ "Party-Race-Religion-Gender",
                              app_identity == 1423 ~ "Party-Gender-Race-Religion",
                              app_identity == 1432 ~ "Party-Race-Gender-Religion",
                              app_identity == 2134 ~ "Religion-Party-Gender-Race",
                              app_identity == 2143 ~ "Religion-Party-Race-Gender",
                              app_identity == 2314 ~ "Gender-Party-Religion-Race",
                              app_identity == 2341 ~ "Race-Party-Religion-Gender",
                              app_identity == 2413 ~ "Gender-Party-Race-Religion",
                              app_identity == 2431 ~ "Race-Party-Gender-Religion",
                              app_identity == 3124 ~ "Religion-Gender-Party-Race",
                              app_identity == 3142 ~ "Religion-Race-Party-Gender",
                              app_identity == 3214 ~ "Gender-Religion-Party-Race",
                              app_identity == 3241 ~ "Race-Religion-Party-Gender",
                              app_identity == 3412 ~ "Gender-Race-Party-Religion",
                              app_identity == 3421 ~ "Race-Gender-Party-Religion",
                              app_identity == 4123 ~ "Religion-Gender-Race-Party",
                              app_identity == 4132 ~ "Religion-Race-Gender-Party",
                              app_identity == 4213 ~ "Gender-Religion-Race-Party",
                              app_identity == 4231 ~ "Race-Religion-Gender-Party",
                              app_identity == 4312 ~ "Gender-Race-Religion-Party",
                              app_identity == 4321 ~ "Race-Gender-Religion-Party"),
         ordering = tolower(ordering))

dt_analysis <- cbind(conf_persons, main)

table(dt_analysis$bn_police, useNA = "always")
table(dt_analysis$bn_reform, useNA = "always")
table(dt_analysis$bn_gun, useNA = "always")
table(dt_analysis$bn_abortion, useNA = "always")
table(dt_analysis$bn_environment, useNA = "always")

# Use marginal rank (naive approach #1)
# Problem: each item shows some relationship, but not as an entire ranking



m1 <- glm(bn_police ~ rank_party + age + gender3 + race4 + educ4 + pid7, 
          dt_analysis, family = binomial)
m2 <- glm(bn_police ~ rank_gender + age + gender3 + race4 + educ4 + pid7,
          dt_analysis, family = binomial)
m3 <- glm(bn_police ~ rank_religion + age + gender3 + race4 + educ4 + pid7, 
          dt_analysis, family = binomial)
m4 <- glm(bn_police ~ rank_race_ethnicity + age + gender3 + race4 + educ4 + pid7,
          dt_analysis, family = binomial)

# Include all marginal ranks
m5 <- glm(bn_police ~ rank_party + rank_gender + rank_religion +
          age + gender3 + race4 + educ4 + pid7, 
          dt_analysis, family = binomial)


summary(m1)
summary(m2)
summary(m3)
summary(m4)

summary(m5)

library(clarify)
set.seed(123)
sim_coefs <- sim(m1)
sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_party = 1:4)
                    )

plot(sim_est) +
  ylim(0.05, 0.4) +
  ylab("Probability") +
  xlab("Marginal Rank (Party)") +
  ggtitle("Without Other Items") -> a



sim_coefs <- sim(m2)
sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_gender = 1:4)
)

plot(sim_est) +
  ylim(0.05, 0.4) +  
  ylab("Probability") +
  xlab("Marginal Rank (Gender)") +
  ggtitle("Without Other Items") -> b


sim_coefs <- sim(m3)
sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_religion = 1:4)
)

plot(sim_est) +
  ylim(0.05, 0.4) +
  ylab("Probability") +
  xlab("Marginal Rank (Religion)") +
  ggtitle("Without Other Items") -> c


sim_coefs <- sim(m4)
sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_race_ethnicity = 1:4
                    )
)

plot(sim_est) +
  ylim(0.05, 0.4) +
  ylab("Probability") +
  xlab("Marginal Rank (Race)") +
  ggtitle("Without Other Items") -> d



sim_coefs <- sim(m5)
sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_party = 1:4)
)

plot(sim_est) +
  ylim(0.05, 0.4) +
  ylab("Probability") +
  xlab("Marginal Rank (Party)") +
  ggtitle("With Other Items") -> a2


sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_gender = 1:4)
)

plot(sim_est) +
  ylim(0.05, 0.4) +
  ylab("Probability") +
  xlab("Marginal Rank (Gender)") +
  ggtitle("With Other Items") -> b2



sim_est <- sim_setx(sim_coefs, 
                    x = list(rank_religion = 1:4)
)

plot(sim_est) +
  ylim(0.05, 0.4) +
  ylab("Probability") +
  xlab("Marginal Rank (Religion)") +
  ggtitle("With Other Items") -> c2


ggpubr::ggarrange(a, b, c, d,
                  a2, b2, c2,
                  ncol = 4, nrow = 2) -> com

annotate_figure(com, top = text_grob("Support for Police Defunding", 
                                     face = "bold", size = 10))

ggsave(here::here("fig/sub", "sub_issue_police.pdf"), width = 8, height = 6)





# dt_analysis %>%
#   ggplot(aes(x = rank_party, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial, formula = bn_police ~ rank_party)) +
#   ylab("") +
#   xlab("party (marginal rank)") +
#   theme_bw() -> a
# 
# dt_analysis %>%
#   ggplot(aes(x = rank_religion, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   ylab("") +  
#   xlab("religion (marginal rank)") +  
#   theme_bw() -> b
# 
# dt_analysis %>%
#   ggplot(aes(x = rank_gender, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   ylab("") +  
#   xlab("gender (marginal rank)") +    
#   theme_bw() -> c
# 
# dt_analysis %>%
#   ggplot(aes(x = rank_race_ethnicity, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   ylab("") +
#   xlab("race (marginal rank)") +      
#   theme_bw() -> d





# Use rankings as categories
library(lme4)

# List of issues
# bn_police
# bn_reform
# bn_gun
# bn_abortion
# bn_environment

m1 <- glmer(bn_police ~ age + gender3 + race4 + educ4 + pid7 + (1 | ordering),
             data = dt_analysis, family = binomial, nAGQ=1)

m2 <- glmer(bn_reform ~ age + gender3 + race4 + educ4 + pid7 + (1 | ordering),
            data = dt_analysis, family = binomial, nAGQ=1)

m3 <- glmer(bn_gun ~ age + gender3 + race4 + educ4 + pid7 + (1 | ordering),
            data = dt_analysis, family = binomial, nAGQ=1)

m4 <- glmer(bn_abortion ~ age + gender3 + race4 + educ4 + pid7 + (1 | ordering),
            data = dt_analysis, family = binomial, nAGQ=1)

m5 <- glmer(bn_environment ~ age + gender3 + race4 + educ4 + pid7 + (1 | ordering),
            data = dt_analysis, family = binomial, nAGQ=1)


df1 <- ranef(m1, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "police")

df2 <- ranef(m2, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "reform")

df3 <- ranef(m3, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "gun")

df4 <- ranef(m4, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "abortion")

df5 <- ranef(m5, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "environment")

gg <- rbind(df1, df2, df3, df4, df5) %>%
  mutate(est = condval,
         lower = est - 1.96*condsd,
         upper = est + 1.96*condsd,
         color = case_when(lower > 0 ~ "positive",
                           upper < 0 ~ "negative",
                           TRUE ~ "not-significant"))

fct_reorder(Species, Sepal.Width)

gg %>%
  ggplot(aes(x = est, y = fct_reorder(grp, est))) +
  geom_point(aes(color = color)) +
  geom_errorbar(aes(xmin=lower, xmax=upper, color = color), width=0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  facet_wrap(~ issue, ncol = 5) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  xlab("Random Effects") +
  ylab("") +
  theme(legend.position = "top")

ggsave(here::here("fig/sub", "sub_issue_police_re.pdf"), width = 9, height = 6)


# pdf(here::here("fig/sub", "sub_issue_police_re.pdf"), width = 9, height = 6)
# lattice::dotplot(ranef(m, which = "ordering", condVar = TRUE))
# dev.off()



# Use D1-2 scores
dt_analysis %>%
ggplot(aes(x = D1, y = bn_police)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  theme_bw() -> a

dt_analysis %>%
  ggplot(aes(x = D2, y = bn_police)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  theme_bw() -> b

# dt_analysis %>%
#   ggplot(aes(x = D1, y = bn_reform)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> c
# 
# dt_analysis %>%
#   ggplot(aes(x = D2, y = bn_reform)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> d
# 
# dt_analysis %>%
#   ggplot(aes(x = D1, y = bn_gun)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> e
# 
# dt_analysis %>%
#   ggplot(aes(x = D2, y = bn_gun)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> f
# 
# 
# dt_analysis %>%
#   ggplot(aes(x = D1, y = bn_abortion)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> g
# 
# dt_analysis %>%
#   ggplot(aes(x = D2, y = bn_abortion)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> h
# 
# 
# dt_analysis %>%
#   ggplot(aes(x = D1, y = bn_environment)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> i
# 
# dt_analysis %>%
#   ggplot(aes(x = D2, y = bn_environment)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   theme_bw() -> j

ggpubr::ggarrange(a, b)

ggsave(here::here("fig/sub", "sub_issue_police_unfold.pdf"), width = 5, height = 2.5)




# Comparison with PID7
dt_analysis %>%
  ggplot(aes(x = pid7, y = bn_abortion)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  theme_bw() 



