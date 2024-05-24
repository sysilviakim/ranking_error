source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
source(here::here("R", "imprr_weights.R"))
source(here::here("R", "imprr_direct.R"))

# Grab main data
main <- df_list$main

# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )


# Generate few variables =====================================================

dt <- main %>%
  filter(!is.na(ideo7)) %>%
  mutate(party_intense = abs(pid7 - 4),
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
         ranking = app_identity,
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
         ordering = tolower(ordering)
         )

# Get bias-correction weights ==================================================

data <- main %>%
  select(app_identity_1, app_identity_2, app_identity_3, app_identity_4,
         anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
         anc_correct_identity)

w <- imprr_weights(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity"
)

print(w)


# Get the corrected PMF of all orderings =======================================

pmf <- dt %>%
  distinct(ranking, ordering) %>%
  arrange(ranking) %>%
  left_join(w$corrected_pmf, by = "ranking") %>%
  select(ordering, prop_renormalized) %>%
  arrange(desc(prop_renormalized))

pmf %>%
  xtable(digits = c(1, 1, 3))

sum(pmf$prop_renormalized) # must be one (check)


# Get the corrected average ranks ==============================================
 
## Corrected average ranks via direct correction
direct <- imprr_direct(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 200
)

direct$qoi %>% filter(qoi == "average rank")
  
## Unweighted (working on weight-adding function)
avg_rank(dt, "app_identity", items = c("Party", "Religion", "Gender", "Race"))


## Merge weights
dt <- dt %>%
  left_join(w$weights, by = "ranking")

## All sample
dt %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         w) %>%
  summarise(wm_gender = weighted.mean(rank_gender, w),
            wm_race = weighted.mean(rank_race, w),
            wm_religion = weighted.mean(rank_religion, w),
            wm_party = weighted.mean(rank_party, w)) 

## Compare
direct$qoi %>% filter(qoi == "average rank")



# By gender
table(dt$gender3)

# 1 = Man
# 2 = Woman
# 3 = Other/Prefer not to say

dt %>%
  group_by(gender3) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         w) %>%
  summarise(wm_gender = weighted.mean(rank_gender, w),
            wm_race = weighted.mean(rank_race, w),
            wm_religion = weighted.mean(rank_religion, w),
            wm_party = weighted.mean(rank_party, w)) %>%
  arrange(desc(wm_gender))


# By race
table(dt$race4)

# 1 = White
# 2 = Black
# 3 = Latino
# 4 = Other

dt %>%
  group_by(race4) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         w) %>%
  summarise(wm_gender = weighted.mean(rank_gender, w),
            wm_race = weighted.mean(rank_race, w),
            wm_religion = weighted.mean(rank_religion, w),
            wm_party = weighted.mean(rank_party, w)) %>%
  arrange(desc(wm_race))


# By religion
table(dt$religpew)

# 1 = Protestant
# 2 = Roman Catholic
# 5 = Jewish
# 9 = Atheist 
# 10 = Agnostic
# 11 = Nothing in particular
# 12 = Something else

dt %>%
  group_by(religpew) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         w) %>%
  summarise(wm_gender = weighted.mean(rank_gender, w),
            wm_race = weighted.mean(rank_race, w),
            wm_religion = weighted.mean(rank_religion, w),
            wm_party = weighted.mean(rank_party, w)) %>%
  arrange(desc(wm_religion))


# By partisanship
table(dt$pid3final)

dt %>%
  group_by(pid3final) %>%
  select(rank_party, rank_gender, rank_race, rank_religion, 
         w) %>%
  summarise(wm_gender = weighted.mean(rank_gender, w),
            wm_race = weighted.mean(rank_race, w),
            wm_religion = weighted.mean(rank_religion, w),
            wm_party = weighted.mean(rank_party, w)) %>%
  arrange(desc(wm_party))


# Modal rankings (not really informative, need to think)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 1 = Man
# 2 = Woman
# 3 = Other/Prefer not to say

Mode(dt$ordering)
Mode(dt$ordering[dt$gender3 == 1])
Mode(dt$ordering[dt$gender3 == 2])
Mode(dt$ordering[dt$gender3 == 3])

Mode(dt$ordering[dt$race4 == 1])
Mode(dt$ordering[dt$race4 == 2])
Mode(dt$ordering[dt$race4 == 3])
Mode(dt$ordering[dt$race4 == 4])

Mode(dt$ordering[dt$religpew == 1])
Mode(dt$ordering[dt$religpew == 2])
Mode(dt$ordering[dt$religpew == 5])
Mode(dt$ordering[dt$religpew == 9])
Mode(dt$ordering[dt$religpew == 10])
Mode(dt$ordering[dt$religpew == 11])
Mode(dt$ordering[dt$religpew == 12])

Mode(dt$ordering[dt$pid3 == 1])
Mode(dt$ordering[dt$pid3 == 2])
Mode(dt$ordering[dt$pid3 == 3])

