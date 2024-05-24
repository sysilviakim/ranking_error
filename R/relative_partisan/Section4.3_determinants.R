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
         ideo7 = ifelse(ideo7 == 8, 4, ideo7), # not sure into moderate         
         newsint = ifelse(newsint == 7, 4, newsint), # not sure into 4 (hardly),
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



# The relationship between party and PID and Ideology

set.seed(413)
dt %>%
  ggplot(aes(x = pid7, y = rank_party)) +
  geom_jitter(width = 0.2, height = 0.2, 
              color = "gray50", alpha = 0.5, pch = 1) +
  geom_smooth(method = "loess", color = "maroon") +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  theme_bw() +
  xlab("Party identification") +
  ylab("Marginal rank of party") -> a


set.seed(413)
dt %>%
  ggplot(aes(x = ideo7, y = rank_party)) +
  geom_jitter(width = 0.2, height = 0.2, 
              color = "gray50", alpha = 0.5, pch = 1) +
  geom_smooth(method = "loess", color = "maroon") +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  theme_bw() +
  xlab("Ideology") +
  ylab("Marginal rank of party") -> b


set.seed(413)
dt %>%
  ggplot(aes(x = newsint, y = rank_party)) +
  geom_jitter(width = 0.2, height = 0.2, 
              color = "gray50", alpha = 0.5, pch = 1) +
  geom_smooth(method = "loess", color = "maroon") +
  scale_x_continuous(breaks=seq(1, 7, 1)) +
  theme_bw() +
  xlab("Political Interest") +
  ylab("Marginal rank of party") -> c

ggpubr::ggarrange(a, b, c, ncol = 3)

ggsave("fig/sub/sub_marginal_party.pdf",
       width = 6, height = 3)

