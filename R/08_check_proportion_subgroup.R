source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# library(questionr) # for wtd.table()
library(RColorBrewer)  

# Grab main data
main <- df_list$main


# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)

data <- main %>%
  select(app_identity_1, app_identity_2, app_identity_3, app_identity_4,
         anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
         anc_correct_identity, weight,
         race4, gender3, pid3, educ4)


# Direct bias correction
d <- imprr_direct(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity",
  weight = data$weight
)


# Race ==============================================================

d_white <- imprr_direct(
  data = data %>% filter(race4 == 1),
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity"
)



d_black <- imprr_direct(
  data = data %>% filter(race4 == 2),
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity"
)



d_latino <- imprr_direct(
  data = data %>% filter(race4 == 3),
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity"
)



d_others <- imprr_direct(
  data = data %>% filter(race4 == 4),
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity"
)


d$est_p_random
# mean     lower     upper
# 1 0.3153146 0.2864261 0.3481958

d_white$est_p_random
# mean     lower     upper
# 1 0.2932953 0.2639125 0.3296256

d_black$est_p_random
# mean     lower     upper
# 1 0.4485607 0.3685907 0.5397301

d_latino$est_p_random
# mean     lower     upper
# 1 0.3547826 0.2879299 0.4282933

d_others$est_p_random
# mean     lower     upper
# 1 0.281966 0.1814745 0.3784499

