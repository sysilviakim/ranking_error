source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# remotes::install_github("sysilviakim/Kmisc")

# Grab rankings and weights
imp_w <- read_csv(here::here("data/tidy", "temp_weight.csv")) %>%
  mutate(app_identity = as.character(ranking),
         bias_weight = weight) %>%
  dplyr::select(app_identity, bias_weight)

imp_w
hist(imp_w$bias_weight, breaks = 20) # 6 rankings should not exist, thus  w = 0

# Grab main data
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
  mutate(id = 1:nrow(main)) %>%
  rename(party = app_identity_1,
         religion = app_identity_2,
         gender = app_identity_3,
         race_ethnicity = app_identity_4) %>%
  left_join(imp_w, by = "app_identity") %>%
  dplyr::select(party, religion, gender, race_ethnicity, id, ideo7, bias_weight) %>%
  filter(!is.na(ideo7))


# Run Unfolding (there is no way to incorporate weights...)
library(smacof)

dt_trans <- dt %>%
  distinct(party, religion, gender, race_ethnicity)

out <- unfolding(dt[,1:4], 
                 type = "ordinal")

plot(out, type = "p", pch = 16, col.columns = "darkcyan",
     label.conf.columns = list(label = TRUE, pos = 3, col = "darkcyan", cex = 1.2),
     col.rows = 8, label.conf.rows = list(label = F, pos = 3, col = 8))
