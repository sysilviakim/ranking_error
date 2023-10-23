source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

main <- df_list$main

# Data processing ==============================================================
# Fix some weird situation
class(main$app_identity_1) <- "numeric"
class(main$app_identity_2) <- "numeric"
class(main$app_identity_3) <- "numeric"
class(main$app_identity_4) <- "numeric"
class(main$ideo7) <- "numeric"

# Reference set: (party, religion, gender, race)

# Downsize data
dt <- main %>%
  mutate(id = 1:nrow(main),
         choice = substr(app_identity, 1, 1),
         choice = case_when(choice == 1 ~ "party",
                            choice == 2 ~ "religion",
                            choice == 3 ~ "gender",
                            choice == 4 ~ "race")) %>%
  select(starts_with("ch"), id, ideo7) %>%
  filter(!is.na(ideo7))

head(dt) # check


# Run Multinomial logit =========================================================

fit_basic <- multinom(choice ~ ideo7, data = dt)
summary(fit_basic)
tidy(fit_basic, conf.int = TRUE)

ggeffect(fit_basic, terms = "ideo7[1:7 by = 1]") %>%
  plot()


