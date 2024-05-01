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
         rank_race_ethnicity = app_identity_4)

dt_analysis <- cbind(conf_persons, main)

table(dt_analysis$bn_police, useNA = "always")
table(dt_analysis$bn_reform, useNA = "always")
table(dt_analysis$bn_gun, useNA = "always")
table(dt_analysis$bn_abortion, useNA = "always")
table(dt_analysis$bn_environment, useNA = "always")

# Use marginal rank (naive approach #1)
# Problem: each item shows some relationship, but not as an entire ranking
dt_analysis %>%
  ggplot(aes(x = rank_party, y = bn_police)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  ylab("") +
  xlab("party (marginal rank)") +
  theme_bw() -> a

dt_analysis %>%
  ggplot(aes(x = rank_religion, y = bn_police)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  ylab("") +  
  xlab("religion (marginal rank)") +  
  theme_bw() -> b

dt_analysis %>%
  ggplot(aes(x = rank_gender, y = bn_police)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  ylab("") +  
  xlab("gender (marginal rank)") +    
  theme_bw() -> c

dt_analysis %>%
  ggplot(aes(x = rank_race_ethnicity, y = bn_police)) + geom_point() +
  stat_smooth(method="glm", color="darkcyan", se=T, 
              method.args = list(family=binomial)) +
  ylab("") +
  xlab("race (marginal rank)") +      
  theme_bw() -> d


ggpubr::ggarrange(a, b, c, d) -> com

annotate_figure(com, top = text_grob("Support for Police Reform(?)", 
                                        face = "bold", size = 10))

ggsave(here::here("fig/sub", "sub_issue_police.pdf"), width = 5, height = 4)


# Use rankings as categories
library(lme4)
m <- glmer(bn_police ~ 1 + (1 | app_identity),
             data = dt_analysis, family = binomial, nAGQ=1)

summary(m)

pdf(here::here("fig/sub", "sub_issue_police_re.pdf"), width = 5, height = 4)
lattice::dotplot(ranef(m, which = "app_identity", condVar = TRUE))
dev.off()



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



