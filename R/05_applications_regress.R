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
  mutate(id = 1:nrow(main)) %>%
  rename(ch.party = app_identity_1,
         ch.religion = app_identity_2,
         ch.gender = app_identity_3,
         ch.race = app_identity_4) %>%
  select(starts_with("ch"), id, ideo7) %>%
  filter(!is.na(ideo7))

head(dt) # check


# Run Rank-order logit =========================================================
library(mlogit)
library(effects)

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
mdat$ideo7     # explanatory variable


# Estimating parameters
m <- mlogit(ch ~ 1 | ideo7,  # Y ~ X_item | X_resp
            mdat,                # Data
            reflevel = "party"   # Base category
            ) 


# Raw result
summary(m)

# Interpret results ============================================================

# Clarify does not support rank-order logit
# Placket-Luce
library(PlackettLuce)







# Fitted values for starter: 1082*3 = 3246 choices
fit <- fitted(m, type = "probabilities")
head(fit)

dt2 <- dt %>% 
  slice(rep(1:n(), each = 3)) %>%
  group_by(id) %>%
  mutate(stage = c("First choice", 
                   "Second choice", 
                   "Third choice")) %>%
  ungroup() 

dt2 <- cbind(dt2, fit) %>%
  data.frame()

# Predicted probabilities in many forms
# (1)  Probability (Party = 1)
# (2A) Probability (Party = 2 | Race = 1)
# (2B) Probability (Party = 2 | Religion = 1)
# (2C) Probability (Party = 2 | Gender = 1)
# (3A) Probability (Party = 3 | (Race, Religion) = 1,2)
# (3B) Probability (Party = 3 | (Gender, Religion) = 1,2)
# (3C) Probability (Party = 3 | (Race, Gender = 1,2)

# What are other two cases?


# Visualize fitted values over ideology
p <-  ggplot(dt2, aes(x = ideo7, y = party)) +
  geom_point(color = "maroon") +
  facet_wrap( ~ stage, nrow = 1) +
  theme_bw() +
  ylab("Pr (Political Party)") +
  xlab("Ideology")

p
ggsave(here::here("fig", "regress.pdf"),
       height = 4, width = 10)


