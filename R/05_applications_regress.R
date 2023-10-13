source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

main <- df_list$main

# Data processing ==============================================================
# Fix some weird situation
class(main$app_identity_1) <- "numeric"
class(main$app_identity_2) <- "numeric"
class(main$app_identity_3) <- "numeric"
class(main$app_identity_4) <- "numeric"

# Reference set: (party, religion, gender, race)

# Downsize data
dt <- main %>%
  mutate(id = 1:nrow(main)) %>%
  rename(ch.party = app_identity_1,
         ch.religion = app_identity_2,
         ch.gender = app_identity_3,
         ch.race = app_identity_4) %>%
  select(starts_with("ch"), id, pid3final)

head(dt) # check


# Run Rank-order logit =========================================================
library(mlogit)


# Transform into mlogit format
dt <- as.data.frame(dt)
mdat <- dfidx::dfidx(dt,             
                     shape = "wide", 
                     choice = "ch", 
                     varying = 1:4, # 1:J, J = # items 
                     ranked = TRUE)
# Check
head(mdat)

mdat$ch        # logical: TRUE if id2 was ranked idx1-th, by unit id1
mdat$id        # respondent id
mdat$idx       # position id (1st, 2nd, 3rd, 4th = depressed)
mdat$pid3final # explanatory variable


# Estimating parameters
m <- mlogit(ch ~ 1 | pid3final,  # Y ~ X_item | X_resp
            mdat,                # Data
            reflevel = "party"   # Base category
            ) 


# Raw result
summary(m)

# Fitted values for starter: 1082*3 = 3246 choices
fit <- fitted(m, type = "probabilities")
head(fit)




# Interpret results ============================================================




