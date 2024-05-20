source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main
item_list <- c(
  "Politicians", "Print Media and TV", "Social Media",
  "Interest Groups", "Citizens"
)

summ_avg_rank_by_group <- function(v = NULL, label = NULL) {
  if (is.null(v) | is.null(label)) {
    avg_rank(main, "app_polar", items = item_list) %>%
      select(-se) %>%
      pivot_wider(names_from = "name", values_from = "mean") %>%
      select(
        Politicians, `Print Media and TV`,
        `Social Media`, `Interest Groups`, Citizens
      )
  } else {
    unique(main[[v]]) %>%
      set_names(., .) %>%
      map(
        ~ avg_rank(
          main %>% filter(!!as.name(v) == .x), "app_polar",
          items = item_list
        )
      ) %>%
      bind_rows(.id = label) %>%
      select(-se) %>%
      pivot_wider(names_from = "name", values_from = "mean") %>%
      select(
        !!as.name(label), Politicians, `Print Media and TV`,
        `Social Media`, `Interest Groups`, Citizens
      )
  }
}

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

# Average rank calculations ====================================================
## By subgroups ----------------------------------------------------------------
## All sample
avg_rank(main, "app_polar", items = item_list)

## By partisanship
summ_avg_rank_by_group("pid3final", "Partisanship")

## By race
summ_avg_rank_by_group("race4labeled", "Race")

## By political interest
summ_avg_rank_by_group(
  "newsint_labeled", 
  "Follow Politics (Political Interest)"
)

## Export via xtable -----------------------------------------------------------
tab <- xtable(summ_avg_rank_by_group(), digits = 2)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks.tex")
)

tab <- xtable(
  summ_avg_rank_by_group("pid3final", "Partisanship") %>%
    mutate(
      Partisanship = factor(
        Partisanship,
        levels = c("Democrat", "Republican", "Independent")
      )
    ),
  digits = 2
)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks_by_partisanship.tex")
)

tab <- xtable(
  summ_avg_rank_by_group("race4labeled", "Race") %>%
    mutate(
      Race = factor(
        Race,
        levels = c("White", "Black", "Hispanic", "Other race"),
        labels = c("White", "Black", "Hispanic/Latino", "Other race")
      )
    ),
  digits = 2
)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks_by_race.tex")
)

tab <- xtable(
  summ_avg_rank_by_group(
    "newsint_labeled", 
    "Follow Politics (Political Interest)"
  ) %>%
    mutate(
      `Follow Politics (Political Interest)` = factor(
        `Follow Politics (Political Interest)`,
        levels = c(
          "Most of the time", "Some of the time",
          "Only now and then", "Hardly at all"
        )
      )
    ),
  digits = 2
)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks_by_political_interest.tex")
)
