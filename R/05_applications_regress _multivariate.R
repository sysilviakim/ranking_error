source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# devtools::install_github("sysilviakim/ranking", force = T)
library(ranking)


# Grab rankings and weights
imp_w <- read_csv(here::here("data/tidy", "temp_weight.csv")) %>%
  mutate(
    app_identity = as.character(ranking),
    bias_weight = weight
  ) %>%
  dplyr::select(app_identity, bias_weight)

imp_w
hist(imp_w$bias_weight, breaks = 20) # 6 rankings should not exist, thus  w = 0

# Grab main data
main <- df_list$main

# Data processing ==============================================================
# Fix some weird situation ---> haven::labelled to numeric
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)

# Downsize data
dt <- main %>%
  mutate(
    id = 1:nrow(main),
    race = as.factor(race),
    region = as.factor(region)
  ) %>%
  rename(
    ch.party = app_identity_1,
    ch.religion = app_identity_2,
    ch.gender = app_identity_3,
    ch.race = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  select(
    starts_with("ch"), id,
    ideo7, pid7, educ, race,
    age, partisan, region,
    bias_weight
  ) %>%
  drop_na()


head(dt) # check
hist(dt$bias_weight, border = "white") # good, no extremely small weight
abline(v = 1, lty = 2, lwd = 2, col = "darkred")


# Run Rank-order logit =========================================================
library(mlogit)

# Transform into mlogit format
dt <- as.data.frame(dt)
mdat <- dfidx::dfidx(
  dt,
  shape = "wide",
  choice = "ch",
  varying = 1:4, # 1:J, J = # items
  ranked = TRUE
)
# Check
head(mdat)

mdat$ch # logical: TRUE if id2 was ranked idx1-th, by unit id1
mdat$id # respondent id
mdat$idx # position id (1st, 2nd, 3rd, 4th = depressed)
mdat$ideo7 # explanatory variable


# Estimating parameters (no weight)
m <- mlogit(
  ch ~ 1 | ideo7 + pid7 + educ + race + age + partisan + region,
  # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender" # Base category
)

# Estimating parameters (with weight)
m2 <- mlogit(
  ch ~ 1 | ideo7 + pid7 + educ + race + age + partisan + region,
  # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender", # Base category
  weight = bias_weight
)

# Raw result
summary(m)
summary(m2)

# save mlogit object for checking
saveRDS(m, file = here::here("output", "full_raw.RData"))
# save mlogit object for checking
saveRDS(m2, file = here::here("output", "full_correct.RData"))
