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


table(main$anc_identity, main$anc_correct_identity) %>%
  xtable()

