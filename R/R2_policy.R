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
  dplyr::select(app_identity, bias_weight)


# # Add bias-corrected weights to raw data
# dt <- main %>%
#   mutate(id = 1:nrow(main)) %>%
#   rename(
#     party = app_identity_1,
#     religion = app_identity_2,
#     gender = app_identity_3,
#     race_ethnicity = app_identity_4
#   ) %>%
#   left_join(imp_w, by = "app_identity") %>%
#   dplyr::select(
#     party, religion, gender, race_ethnicity, id, ideo7, bias_weight
#   ) %>%
#   filter(!is.na(ideo7))


# Generate few variables =====================================================

main <- main %>%
  filter(!is.na(ideo7)) %>%
  mutate(bn_police = ifelse(police == 1, 1, 0),             # issue
         bn_reform = ifelse(reform == 1, 1, 0),             # issue
         bn_gun = ifelse(gun == 1 , 1, 0),                  # issue
         bn_abortion = ifelse(abortion == 1, 1, 0),         # issue
         bn_environment = ifelse(environment == 1, 1, 0),   # issue
         bn_turnout20 = ifelse(turnout20post == 1, 1, 0),     # behavior
         bn_turnout22 = ifelse(turnout22post == 1, 1, 0),     # behavior
         bn_presvote20 = ifelse(presvote20post == 1, 1, 0),   # behavior
         bn_housevote22 = ifelse(housevote22post == 1, 1, 0), # behavior
         bn_regist = ifelse(votereg2 == 1, 1, 0),             # behavior
         age_sq = age^2,
         party_intense = abs(pid7 - 4),
         rank_party = app_identity_1,
         rank_religion = app_identity_2,
         rank_gender = app_identity_3,
         rank_race = app_identity_4,
         top1 = case_when(rank_party == 1 ~ "party",
                          rank_religion == 1 ~ "religion",
                          rank_gender == 1 ~ "gender",
                          rank_race == 1 ~ "race"),
         pid7 = ifelse(pid7 == 8, 4, pid7), # not sure into indep
         gender3 = ifelse(gender3 == 4, 3, gender3), # prefer not to say into other
         religpew = ifelse(religpew %in% c(3,4,6,7,8), 12, religpew), # several into something else
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
         ordering = tolower(ordering)) %>%
  left_join(imp_w, by = "app_identity") 

dt <- main


# # check
# par(mfrow=c(2,1))
# hist(imp_w$bias_weight, breaks = 20)
# hist(main$bias_weight, breaks = 20)


table(dt$bn_police, useNA = "always")
table(dt$bn_reform, useNA = "always")
table(dt$bn_gun, useNA = "always")
table(dt$bn_abortion, useNA = "always")
table(dt$bn_environment, useNA = "always")

# Descriptive statistics
# Bias-corrected PMF
library(questionr)
wtd.table(x = dt$ordering, 
          weights = dt$bias_weight) %>%
   as.data.frame() %>%
   arrange(desc(Freq)) %>%
   mutate(pmf = Freq/sum(dt$bias_weight)) -> w.pmf


wtd.table(x = dt$ordering) %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  mutate(pmf = Freq/dim(dt)[1]) -> r.pmf

# Check
w.pmf  # bias-corrected
r.pmf  # raw data
sum(w.pmf$pmf) 
sum(r.pmf$pmf) 

w.pmf[,-2] %>%
  xtable(digits = 3)

# Average ranks
## Raw data
dt %>%
  select(rank_party, rank_gender, rank_race, rank_religion) %>%
  summarise_all(mean) %>%
  xtable()

## Bias-corrected
dt %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         bias_weight) %>%
  summarise(wm_gender = weighted.mean(rank_gender, bias_weight),
            wm_race = weighted.mean(rank_race, bias_weight),
            wm_religion = weighted.mean(rank_religion, bias_weight),
            wm_party = weighted.mean(rank_party, bias_weight)) %>%
  xtable()

# By partisanship
dt %>%
  group_by(pid3final) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         bias_weight) %>%
  summarise(wm_gender = weighted.mean(rank_gender, bias_weight),
            wm_race = weighted.mean(rank_race, bias_weight),
            wm_religion = weighted.mean(rank_religion, bias_weight),
            wm_party = weighted.mean(rank_party, bias_weight))

dt %>%
  group_by(race4) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         bias_weight) %>%
  summarise(wm_gender = weighted.mean(rank_gender, bias_weight),
            wm_race = weighted.mean(rank_race, bias_weight),
            wm_religion = weighted.mean(rank_religion, bias_weight),
            wm_party = weighted.mean(rank_party, bias_weight))

# 1 = White
# 2 = Black
# 3 = Latino
# 4 = Other

# By gender
dt %>%
  group_by(gender3) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         bias_weight) %>%
  summarise(wm_gender = weighted.mean(rank_gender, bias_weight),
            wm_race = weighted.mean(rank_race, bias_weight),
            wm_religion = weighted.mean(rank_religion, bias_weight),
            wm_party = weighted.mean(rank_party, bias_weight))

# 1 = Man
# 2 = Woman
# 3 = Other/Prefer not to say

# By religion
dt %>%
  group_by(religpew) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         bias_weight) %>%
  summarise(wm_gender = weighted.mean(rank_gender, bias_weight),
            wm_race = weighted.mean(rank_race, bias_weight),
            wm_religion = weighted.mean(rank_religion, bias_weight),
            wm_party = weighted.mean(rank_party, bias_weight))

# 1 = Protestant
# 2 = Roman Catholic
# 5 = Jewish
# 9 = Atheist 
# 10 = Agnostic
# 11 = Nothing in particular
# 12 = Something else


dt %>%
  ggplot(aes(x = pid7, y = rank_party)) +
  geom_jitter()


##################################################################
# Predicting issue preference and behavioral outcomes
##################################################################

# Use marginal rank (naive approach #1)
# Problem: each item shows some relationship, but not as an entire ranking

library(stats)
library(clarify)

mr_effect <- function(outcome,
                      iv,
                      dt,
                      title){

new_var <-  dt[,iv] %>% pull()   
dt$iv <- new_var

  
# Prep a list of control variables  
var_cont <- "age + age_sq + educ4 + pid3final + party_intense + ideo7 + newsint + 
             as.factor(gender3) + as.factor(race4) + as.factor(region) +
             as.factor(faminc_new) + as.factor(religpew)"  


# Creating a formula using reformulate()
formula <- reformulate(c("iv", var_cont), 
                       response = outcome)
  
# Fitting a logistic regression model
m <- glm(formula,
         data = dt, 
         family = binomial,
         weights = bias_weight)    

# # Unweighted
# m <- glm(formula,
#          data = dt, 
#          family = binomial)    

# Generate predicted probabilities
set.seed(123)
sim_coefs <- sim(m)
sim_est <- sim_setx(sim_coefs, 
                    x = list(iv = 1:4,
                             pid3final = c("Democrat",
                                           "Independent",
                                           "Republican"),
                             gender3 = median(dt$gender3),
                             race4 = median(dt$race4),
                             region = median(dt$region),
                             faminc_new = median(dt$faminc_new),
                             religpew = median(dt$religpew)
                             ))

p <- plot(sim_est) +
  ylim(0, 1) +
  ylab("") +
  xlab(paste0(iv)) +
  scale_color_manual(breaks = c("Democrat", "Independent", "Republican"),
                     values=c("darkcyan", "purple4", "deeppink4")) +  
  theme(legend.position = "none") +
  ggtitle(title) 
  
return(p)
}


# Issues

m1 <- mr_effect("bn_abortion", "rank_party", dt, "abortion")
m2 <- mr_effect("bn_abortion", "rank_gender", dt, "abortion")
m3 <- mr_effect("bn_abortion", "rank_race", dt, "abortion")
m4 <- mr_effect("bn_abortion", "rank_religion", dt, "abortion")
m5 <- mr_effect("bn_police", "rank_party", dt, "police defunding")
m6 <- mr_effect("bn_police", "rank_gender", dt, "police defunding")
m7 <- mr_effect("bn_police", "rank_race", dt, "police defunding")
m8 <- mr_effect("bn_police", "rank_religion", dt, "police defunding")
m9 <- mr_effect("bn_gun", "rank_party", dt, "ban assault rifles")
m10 <- mr_effect("bn_gun", "rank_gender", dt, "ban assault rifles")
m11 <- mr_effect("bn_gun", "rank_race", dt, "ban asault rifles")
m12 <- mr_effect("bn_gun", "rank_religion", dt, "ban assault rifles")
m13 <- mr_effect("bn_reform", "rank_party", dt, "election reforms")
m14 <- mr_effect("bn_reform", "rank_gender", dt, "election reforms")
m15 <- mr_effect("bn_reform", "rank_race", dt, "election reforms")
m16 <- mr_effect("bn_reform", "rank_religion", dt, "election reforms")
m17 <- mr_effect("bn_environment", "rank_party", dt, "strengthen EPA")
m18 <- mr_effect("bn_environment", "rank_gender", dt, "strengthen EPA")
m19 <- mr_effect("bn_environment", "rank_race", dt, "strengthen EPA")
m20 <- mr_effect("bn_environment", "rank_religion", dt, "strengthen EPA")


ggpubr::ggarrange(m1, m2, m3, m4,
                  m9, m10, m11, m12,
                  m13, m14, m15, m16,                  
                  m5, m6, m7, m8,
                  m17, m18, m19, m20,
                  ncol = 4, nrow = 5) 

ggsave(here::here("fig/sub", "sub_issues.pdf"), width = 8, height = 10)
ggsave(here::here("fig/sub", "sub_issues_w.pdf"), width = 8, height = 10)


# Behavior

m1 <- mr_effect("bn_regist", "rank_party", dt, "registration")
m2 <- mr_effect("bn_regist", "rank_gender", dt, "registration")
m3 <- mr_effect("bn_regist", "rank_race", dt, "registration")
m4 <- mr_effect("bn_regist", "rank_religion", dt, "registration")
m5 <- mr_effect("bn_turnout20", "rank_party", dt, "vote 2020")
m6 <- mr_effect("bn_turnout20", "rank_gender", dt, "vote 2020")
m7 <- mr_effect("bn_turnout20", "rank_race", dt, "vote 2020")
m8 <- mr_effect("bn_turnout20", "rank_religion", dt, "vote 2020")
m9 <- mr_effect("bn_turnout22", "rank_party", dt, "vote 2022")
m10 <- mr_effect("bn_turnout22", "rank_gender", dt, "vote 2022")
m11 <- mr_effect("bn_turnout22", "rank_race", dt, "vote 2022")
m12 <- mr_effect("bn_turnout22", "rank_religion", dt, "vote 2022")
m13 <- mr_effect("bn_presvote20", "rank_party", dt, "pres Biden 2020")
m14 <- mr_effect("bn_presvote20", "rank_gender", dt, "pres Biden 2020")
m15 <- mr_effect("bn_presvote20", "rank_race", dt, "pres Biden 2020")
m16 <- mr_effect("bn_presvote20", "rank_religion", dt, "pres Biden 2020")
m17 <- mr_effect("bn_housevote22", "rank_party", dt, "house Dem 2022")
m18 <- mr_effect("bn_housevote22", "rank_gender", dt, "house Dem 2022")
m19 <- mr_effect("bn_housevote22", "rank_race", dt, "house Dem 2022")
m20 <- mr_effect("bn_housevote22", "rank_religion", dt, "house Dem 2022")


ggpubr::ggarrange(m1, m2, m3, m4,
                  m5, m6, m7, m8,
                  m9, m10, m11, m12,
                  m13, m14, m15, m16,                  
                  m17, m18, m19, m20,
                  ncol = 4, nrow = 5) 

ggsave(here::here("fig/sub", "sub_behaviors.pdf"), width = 8, height = 10)
ggsave(here::here("fig/sub", "sub_behaviors_w.pdf"), width = 8, height = 10)




# dt %>%
#   ggplot(aes(x = rank_party, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial, formula = bn_police ~ rank_party)) +
#   ylab("") +
#   xlab("party (marginal rank)") +
#   theme_bw() -> a
# 
# dt %>%
#   ggplot(aes(x = rank_religion, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   ylab("") +  
#   xlab("religion (marginal rank)") +  
#   theme_bw() -> b
# 
# dt %>%
#   ggplot(aes(x = rank_gender, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   ylab("") +  
#   xlab("gender (marginal rank)") +    
#   theme_bw() -> c
# 
# dt %>%
#   ggplot(aes(x = rank_race, y = bn_police)) + geom_point() +
#   stat_smooth(method="glm", color="darkcyan", se=T, 
#               method.args = list(family=binomial)) +
#   ylab("") +
#   xlab("race (marginal rank)") +      
#   theme_bw() -> d





# Use top-1 choice as categories
library(lme4)

# # Issue outcomes
# m1 <- glmer(bn_police ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew) + as.factor(region)  + 
#               (1 | top1),
#              data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m2 <- glmer(bn_reform ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m3 <- glmer(bn_gun ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region) + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m4 <- glmer(bn_abortion ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m5 <- glmer(bn_environment ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# 
# # Behavioral outcomes
# m6 <- glmer(bn_turnout20 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m7 <- glmer(bn_turnout22 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m8 <- glmer(bn_presvote20 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# m9 <- glmer(bn_housevote22 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# 
# m10 <- glmer(bn_regist ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
#               educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
#               (1 | top1),
#             data = dt, family = binomial, weight = bias_weight, nAGQ=1)
# 
# 
# 
# df1 <- ranef(m1, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "police")
# 
# df2 <- ranef(m2, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "reform")
# 
# df3 <- ranef(m3, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "gun")
# 
# df4 <- ranef(m4, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "abortion")
# 
# df5 <- ranef(m5, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "environment")
# 
# df6 <- ranef(m6, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "turnout 2020")
# 
# df7 <- ranef(m7, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "turnout 2022")
# 
# df8 <- ranef(m8, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "pres vote 2020")
# 
# df9 <- ranef(m9, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "house vote 2022")
# 
# df10 <- ranef(m10, which = "top1", condVar = TRUE) %>%
#   as.data.frame() %>%
#   mutate(issue = "registration 2020")
# 
# 
# alpha <- 0.05
# 
# gg <- rbind(df1, df2, df3, df4, df5,
#             df6, df7, df8, df9, df10) %>%
#   mutate(est = condval,
#          lower = est - qnorm(1-alpha/2)*condsd,
#          upper = est + qnorm(1-alpha/2)*condsd,
#          color = case_when(lower > 0 ~ "positive",
#                            upper < 0 ~ "negative",
#                            TRUE ~ "not-significant"))
# 
# gg$issue <- factor(gg$issue, 
#                    levels=c('abortion', 'environment', 'gun', 'police', 'reform',
#                             'registration 2020', 'turnout 2020', 'turnout 2022', 
#                             'pres vote 2020', 'house vote 2022'))
# 
# 
# # I don't know how to reorder "grp" by the values of "est" for "abortion" only
# gg %>%
#   ggplot(aes(x = est, y = fct_reorder(grp, est))) +
#   geom_point(aes(color = color)) +
#   geom_errorbar(aes(xmin=lower, xmax=upper, color = color), width=0) + 
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
#   facet_wrap(~ issue, ncol = 5, nrow = 2) +
#   scale_color_brewer(palette="Set1") +
#   theme_bw() +
#   xlab("Random Effects") +
#   ylab("") +
#   theme(legend.position = "top")
# 
# ggsave(here::here("fig/sub", "sub_random_effects_top1.pdf"), width = 10, height = 5)
# 
# 
# # pdf(here::here("fig/sub", "sub_issue_police_re.pdf"), width = 9, height = 6)
# # lattice::dotplot(ranef(m, which = "ordering", condVar = TRUE))
# # dev.off()



# Use ranking profiles as a categorical independent variable

library(lme4)

# Issue outcomes
m1 <- glmer(bn_police ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew) + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m2 <- glmer(bn_reform ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m3 <- glmer(bn_gun ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region) + 
              faminc_new + as.factor(religpew) + party_intense +
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m4 <- glmer(bn_abortion ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m5 <- glmer(bn_environment ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +              
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)


# Behavioral outcomes
m6 <- glmer(bn_turnout20 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +              
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m7 <- glmer(bn_turnout22 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m8 <- glmer(bn_presvote20 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +              
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)

m9 <- glmer(bn_housevote22 ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
              educ4 + pid7 + ideo7 + newsint + as.factor(religpew)  + as.factor(region)  + 
              faminc_new + as.factor(religpew) + party_intense +              
              (1 | ordering),
            data = dt, family = binomial, weights = bias_weight, nAGQ=1)


m10 <- glmer(bn_regist ~ age + age_sq + as.factor(gender3) + as.factor(race4) + 
               educ4 + pid7 + ideo7 + newsint  + as.factor(region)  + 
               faminc_new + as.factor(religpew) + party_intense +               
               (1 | ordering),
             data = dt, family = binomial, weights = bias_weight, nAGQ=1)



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

df6 <- ranef(m6, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "turnout 2020")

df7 <- ranef(m7, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "turnout 2022")

df8 <- ranef(m8, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "pres vote 2020")

df9 <- ranef(m9, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "house vote 2022")

df10 <- ranef(m10, which = "ordering", condVar = TRUE) %>%
  as.data.frame() %>%
  mutate(issue = "registration 2020")


alpha <- 0.05

gg <- rbind(df1, df2, df3, df4, df5,
            df6, df7, df8, df9, df10) %>%
  mutate(est = condval,
         lower = est - qnorm(1-alpha/2)*condsd,
         upper = est + qnorm(1-alpha/2)*condsd,
         color = case_when(lower > 0 ~ "positive",
                           upper < 0 ~ "negative",
                           TRUE ~ "not-significant"))

gg$issue <- factor(gg$issue, 
                   levels=c('abortion', 'environment', 'gun', 'police', 'reform',
                            'registration 2020', 'turnout 2020', 'turnout 2022', 
                            'pres vote 2020', 'house vote 2022'))


# I don't know how to reorder "grp" by the values of "est" for "abortion" only
gg %>%
  ggplot(aes(x = est, y = fct_reorder(grp, est))) +
  geom_point(aes(color = color)) +
  geom_errorbar(aes(xmin=lower, xmax=upper, color = color), width=0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  facet_wrap(~ issue, ncol = 5, nrow = 2) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  xlab("Random Effects") +
  ylab("") +
  theme(legend.position = "top")

ggsave(here::here("fig/sub", "sub_random_effects.pdf"), width = 10, height = 9)



# # =========================#
# # Use D1-2 scores
# # This approach seems not productive
# # =========================#
# 
# d1 <- glm(bn_abortion ~ D1 + 
#             age + age_sq + as.factor(gender3) + as.factor(race4) + 
#             educ4 + pid7 + ideo7 + newsint + as.factor(region), 
#           dt, family = binomial)
# 
# d2 <- glm(bn_abortion ~ D2 + 
#             age + age_sq + as.factor(gender3) + as.factor(race4) + 
#             educ4 + pid7 + ideo7 + newsint + as.factor(region), 
#           dt, family = binomial)
# 
# d3 <- glm(bn_abortion ~ D1 + D2 + 
#             age + age_sq + as.factor(gender3) + as.factor(race4) + 
#             educ4 + pid7 + ideo7 + newsint + as.factor(region), 
#           dt, family = binomial)
# 
# 
# # min(dt$D1) # -0.7602498
# # max(dt$D1) # 0.6493996
# # min(dt$D2) # -0.2835745
# # max(dt$D2) # 0.9388381 
# 
# sim_coefs <- sim(d1)
# sim_est <- sim_setx(sim_coefs, 
#                     x = list(D1 = seq(from = -0.8, to = 0.8, by = 0.01),
#                              gender3 = median(dt$gender3),
#                              race4 = median(dt$race4),
#                              region = median(dt$region)))
# 
# plot(sim_est) +
#   ylim(0.25, 1) +
#   ylab("Probability") +
#   xlab("D1 via Unfolding") +
#   ggtitle("") -> a
# 
# 
# sim_coefs <- sim(d2)
# sim_est <- sim_setx(sim_coefs, 
#                     x = list(D2 = seq(from = -0.3, to = 1, by = 0.01),
#                              gender3 = median(dt$gender3),
#                              race4 = median(dt$race4),
#                              region = median(dt$region)))
# 
# plot(sim_est) +
#   ylim(0.25, 1) +
#   ylab("Probability") +
#   xlab("D2 via Unfolding") +
#   ggtitle("") -> b
# 
# 
# sim_coefs <- sim(d3)
# sim_est <- sim_setx(sim_coefs, 
#                     x = list(D1 = seq(from = -0.8, to = 0.8, by = 0.01),
#                              gender3 = median(dt$gender3),
#                              race4 = median(dt$race4),
#                              region = median(dt$region)))
# 
# plot(sim_est) +
#   ylim(0.25, 1) +
#   ylab("Probability") +
#   xlab("D1 via Unfolding") +
#   ggtitle("With D1 and D2") -> c
# 
# 
# 
# sim_est <- sim_setx(sim_coefs, 
#                     x = list(D2 = seq(from = -0.3, to = 1, by = 0.01),
#                              gender3 = median(dt$gender3),
#                              race4 = median(dt$race4),
#                              region = median(dt$region)))
# 
# plot(sim_est) +
#   ylim(0.25, 1) +
#   ylab("Probability") +
#   xlab("D2 via Unfolding") +
#   ggtitle("With D1 and D2") -> d
# 
# 
# 
# ggpubr::ggarrange(a, b, c, d,
#                   ncol = 2, nrow = 2) -> com
# 
# annotate_figure(com, top = text_grob("Support for Abortion", 
#                                      face = "bold", size = 15))
# 
# ggsave(here::here("fig/sub", "sub_issue_abortion_unfolding.pdf"), width = 10, height = 6)
# 
# 
# # dt %>%
# #   ggplot(aes(x = D1, y = bn_reform)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> c
# # 
# # dt %>%
# #   ggplot(aes(x = D2, y = bn_reform)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> d
# # 
# # dt %>%
# #   ggplot(aes(x = D1, y = bn_gun)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> e
# # 
# # dt %>%
# #   ggplot(aes(x = D2, y = bn_gun)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> f
# # 
# # 
# # dt %>%
# #   ggplot(aes(x = D1, y = bn_abortion)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> g
# # 
# # dt %>%
# #   ggplot(aes(x = D2, y = bn_abortion)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> h
# # 
# # 
# # dt %>%
# #   ggplot(aes(x = D1, y = bn_environment)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> i
# # 
# # dt %>%
# #   ggplot(aes(x = D2, y = bn_environment)) + geom_point() +
# #   stat_smooth(method="glm", color="darkcyan", se=T, 
# #               method.args = list(family=binomial)) +
# #   theme_bw() -> j
# 
# ggpubr::ggarrange(a, b)
# 
# ggsave(here::here("fig/sub", "sub_issue_police_unfold.pdf"), width = 5, height = 2.5)
# 
