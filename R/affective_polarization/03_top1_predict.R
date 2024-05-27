source(here::here("R/affective_polarization", "01_setup.R"))
library(lmtest)
library(sandwich)
library(stargazer)

# Top-1 binary variables =======================================================
## What is the proportion of each?
main %>%
  select(matches("_top$")) %>%
  map_dbl(~ round(mean(.x) * 100, digits = 1))

# politician_top      media_top     social_top   interest_top    citizen_top 
#           43.3           24.1           14.8           11.0            6.8

# OLS with each as dependent variable ==========================================
## map with as.formula is trouble when trying to do vcovHC
temp <- list(
  politician = lm(
    politician_top ~ pid3final + ideo7 + party_intense + age + 
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main
  ),
  media = lm(
    media_top ~ pid3final + ideo7 + party_intense + age + 
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main
  ),
  social = lm(
    social_top ~ pid3final + ideo7 + party_intense + age + 
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main
  ),
  interest = lm(
    interest_top ~ pid3final + ideo7 + party_intense + age + 
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main
  ),
  citizen = lm(
    citizen_top ~ pid3final + ideo7 + party_intense + age + 
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main
  )
) %>%
  ## Rename variables
  map(
    ~ {
      names(.x$coefficients) <- names(.x$coefficients) %>%
        str_replace_all("age", "Age") %>%
        str_replace_all("Age_sq", "Age squared") %>%
        str_replace_all("gender3Female", "Female") %>%
        str_replace_all("gender3Other", "Neither male/female") %>%
        str_replace_all("race4labeledBlack", "Black") %>%
        str_replace_all("race4labeledHispanic", "Hispanic/Latino") %>%
        str_replace_all("race4labeledOther race", "Other race") %>%
        str_replace_all("educ4Some college", "Some college") %>%
        str_replace_all("educ4College grad", "College graduate") %>%
        str_replace_all("educ4Postgrad", "Post-graduate") %>%
        str_replace_all("pid3finalIndependent", "Independent") %>%
        str_replace_all("pid3finalRepublican", "Republican") %>%
        str_replace_all("ideo7", "Ideology") %>%
        str_replace_all("newsint", "Political interest") %>%
        str_replace_all("regionSouth", "Region: south") %>%
        str_replace_all("regionWest", "Region: west") %>%
        str_replace_all("regionMidwest", "Region: midwest") %>%
        str_replace_all("ideo7Moderate", "Moderate") %>%
        str_replace_all("faminc50-80k", "Household income: 50-80k") %>%
        str_replace_all("faminc80-150k", "Household income: 80-150k") %>%
        str_replace_all(
          "faminc150k or more",
          "Household income: 150k or more"
        ) %>%
        str_replace_all(
          "famincPrefer not to say",
          "Household income: prefer not to say"
        ) %>%
        str_replace_all("relig5Catholic", "Catholic") %>%
        str_replace_all("relig5Jewish", "Jewish") %>%
        str_replace_all(
          "relig5None/Agnostic/Atheist",
          "None/Agnostic/Atheist"
        ) %>%
        str_replace_all("relig5Other", "Other religion") %>%
        str_replace_all("party_intense", "Party ID Intensity")
      return(.x)
    }
  ) %>%
  map(
    ~ list(
      model = .x,
      se_robust = coeftest(.x, vcov = vcovHC(.x, type = "HC0"))[, "Std. Error"]
    )
  )

stargazer(
  temp %>% map("model"),
  se = temp %>% map("se_robust"),
  omit = "Constant", dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE,
  column.labels = c(
    "Politicians", "Print Media and TV", "Social Media",
    "Interest Groups", "Citizens"
  ),
  dep.var.caption = paste0(
    "Linear Regression: ",
    "Which Entity is Most Responsible for Affective Polarization?"
  ),
  out = here("tab", "predict_top1_polar.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE
)

## What is the predictive performance in terms of accuracy? ====================
temp %>%
  imap_dbl(
    ~ {
      pred <- predict(.x$model, type = "response") %>%
        round() %>%
        as.numeric()
      acc <- sum(.x$model$model[[paste0(.y, "_top")]] == pred) / 
        nrow(.x$model$model)
      return(round(acc * 100, digits = 1))
    }
  )

## Have to do AUC, not accuracy;
## anyway, not better than random guess using the proportion
# politician      media     social   interest    citizen 
#       60.0       75.9       85.2       89.0       93.2 
