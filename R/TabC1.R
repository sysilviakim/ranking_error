source(here::here("R", "utilities.R"))
library(lmtest)
library(sandwich)
library(stargazer)
library(fixest)
library(coefplot)

# Setup and load saved .Rda files ==============================================
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main
item_list <- data.frame(
  item = c(
    "Politicians", "Print Media and TV", "Social Media",
    "Interest Groups", "Citizens"
  ),
  variable = paste("app_polar", 1:5, sep = "_")
)

# Data processing ==============================================================
main <- main %>%
  ## One missing case in `ideo7`
  filter(!is.na(ideo7)) %>%
  ## Create new auxiliary variables e.g., support of policies and demographics
  mutate(
    ## Policy support: five issues
    bn_police = ifelse(police == 1, 1, 0),
    bn_reform = ifelse(reform == 1, 1, 0),
    bn_gun = ifelse(gun == 1, 1, 0),
    bn_abortion = ifelse(abortion == 1, 1, 0),
    bn_environment = ifelse(environment == 1, 1, 0),
    ## Political behavior: turnout, vote, and registration
    bn_turnout20 = ifelse(turnout20post == 1, 1, 0),
    bn_turnout22 = ifelse(turnout22post == 1, 1, 0),
    bn_presvote20 = ifelse(presvote20post == 1, 1, 0),
    bn_housevote22 = ifelse(housevote22post == 1, 1, 0),
    bn_regist = ifelse(votereg2 == 1, 1, 0),
    ## Age squared
    age_sq = age^2,
    ## Partisan identity's intensity
    party_intense = abs(pid7 - 4),
    ## Collapse "not sure"s in partisan identity and ideology into
    ## independents and moderates respectively
    pid7 = ifelse(pid7 == 8, 4, pid7),
    ideo7 = ifelse(ideo7 == 8, 4, ideo7),
    ## Collapse "not sure" about political interest into "hardly at all"
    newsint = ifelse(newsint == 7, 4, newsint),
    newsint_labeled = case_when(
      newsint == 1 ~ "Most of the time",
      newsint == 2 ~ "Some of the time",
      newsint == 3 ~ "Only now and then",
      TRUE ~ "Hardly at all"
    ),
    ## If "prefer not to say" in gender, collapse into "other"
    gender3 = ifelse(gender3 == 4, 3, gender3),
    ## Collapsed religion
    relig5 = case_when(
      ## Mormon, Orthodox, Muslim, Buddhist, Hindu:
      ## too few cases to analyze separately
      religpew %in% c(3, 4, 6, 7, 8) ~ 12,
      ## Collapse atheist, agnostic, nothing in particular
      religpew %in% c(9, 10, 11) ~ 11,
      ## Note that Jewish: only 39 cases, but still keep as category
      TRUE ~ as.numeric(religpew)
    ),
    relig5 = factor(
      relig5,
      levels = c(1, 2, 5, 11, 12),
      labels = c(
        "Protestant", "Catholic", "Jewish",
        "None/Agnostic/Atheist", "Other"
      )
    ),
    region = factor(
      region,
      levels = c(1, 2, 3, 4),
      labels = c("Northeast", "Midwest", "South", "West")
    ),
    educ4 = factor(
      educ4,
      levels = c(1, 2, 3, 4),
      labels = c("HS or less", "Some college", "College grad", "Postgrad")
    ),
    gender3 = factor(
      gender3,
      levels = c(1, 2, 3),
      label = c("Male", "Female", "Other")
    ),
    race4labeled = factor(
      race4labeled,
      levels = c("White", "Black", "Hispanic", "Other race"),
    ),
    white = case_when(
      race4labeled != "White" ~ "Non-white",
      TRUE ~ "White"
    ),
    white = factor(
      white, levels = c("White", "Non-white")
    ),
    white_numeric = case_when(
      race4labeled == "White" ~ 0,
      TRUE ~ 1
    ),
    pid3final = factor(
      pid3final,
      levels = c("Democrat", "Independent", "Republican")
    ),
    faminc = case_when(
      faminc_new %in% c(1, 2, 3, 4, 5) ~ "Under 50k",
      faminc_new %in% c(6, 7, 8) ~ "50-80k",
      faminc_new %in% c(9, 10, 11) ~ "80-150k",
      faminc_new %in% c(12, 13, 14, 15, 16) ~ "150k or more",
      TRUE ~ "Prefer not to say"
    ),
    faminc = factor(
      faminc,
      levels = c(
        "Under 50k", "50-80k", "80-150k", "150k or more", "Prefer not to say"
      )
    ),
    age4labeled = factor(
      age4,
      levels = c(1, 2, 3, 4),
      labels = c("Under 30", "30-44", "45-64", "65 and older")
    )
  ) %>%
  ## Recoding identity variables
  mutate(
    ## How does the respondent rank each entity when it comes to blaming
    ## affective polarization?
    rank_politicians = app_polar_1,
    rank_media = app_polar_2,
    rank_social = app_polar_3,
    rank_interest = app_polar_4,
    rank_citizens = app_polar_5,
    top1 = case_when(
      rank_politicians == 1 ~ "politicians",
      rank_media == 1 ~ "print media and TV",
      rank_social == 1 ~ "social media",
      rank_interest == 1 ~ "interest groups",
      rank_citizens == 1 ~ "citizens"
    )
  ) %>%
  mutate(
    politician_top = case_when(app_polar_1 == 1 ~ 1, TRUE ~ 0),
    media_top = case_when(app_polar_2 == 1 ~ 1, TRUE ~ 0),
    social_top = case_when(app_polar_3 == 1 ~ 1, TRUE ~ 0),
    interest_top = case_when(app_polar_4 == 1 ~ 1, TRUE ~ 0),
    citizen_top = case_when(app_polar_5 == 1 ~ 1, TRUE ~ 0)
  )

# Descriptive statistics =======================================================
prop(main, "pid3final")
prop(main, "race4labeled")
prop(main, "gender3")
prop(main, "educ4")
prop(main, "faminc")
summary(main$age)
prop(main, "age4labeled")

# What determines a random response? OLS =======================================
temp <- list(
  repeated = lm(
    repeat_identity ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  berinsky_fail = lm(
    berinsky_fail ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  ternovski_fail = lm(
    ternovski_fail ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  anchor_main = lm(
    random_identity ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  exact = lm(
    random_id_exact ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  alphabet = lm(
    random_id_alphabet ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  )
) %>%
  coef_rename()

## Export
## Table C.1, formerly predict_random_identity.tex
## I like booktabs, so changed hlines to 
## toprule, cmidrule, midrule, and bottomrule
stargazer(
  temp %>% map("model"),
  se = temp %>% map("se_robust"),
  omit = "Constant", dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE,
  column.labels = c(
    "Repeated", "Attention I", "Attention II",
    "Anchor main", "Anchor exact", "Anchor alphabet"
  ),
  dep.var.caption = "What Determines a Random Response?",
  out = here("tab", "TabC1.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE
)
