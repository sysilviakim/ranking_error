source(here::here("R", "utilities.R"))

# Setup and load saved .Rda files ==============================================  
load(here("data", "tidy", "df_list.Rda"))
load(here("data", "tidy", "prep_applications_list.Rda"))
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
  )
