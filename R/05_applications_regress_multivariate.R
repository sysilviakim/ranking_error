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
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

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
    age, partisan, region, gender3,
    bias_weight
  ) %>%
  drop_na() %>%
  ## Aggregate Native American, Other, and Middle Eastern into a single category
  ## for race (otherwise, singularity issue)
  mutate(
    race6 = case_when(
      as.numeric(race) %in% c(5, 7, 8) ~ 5,
      TRUE ~ as.numeric(race)
    ),
    race6 = as.factor(race6)
  )

head(dt) # check
hist(dt$bias_weight, border = "white") # good, no extremely small weight
abline(v = 1, lty = 2, lwd = 2, col = "darkred")

# Create indexed data ==========================================================
library(mlogit)

# Transform into mlogit format
dt <- as.data.frame(dt)

## Takes about 2 seconds
mdat <- dfidx::dfidx(
  dt,
  shape = "wide",
  choice = "ch",
  varying = 1:4, # 1:J, J = # items
  ranked = TRUE
)
# Check
head(mdat)

bootstrapped_mdat <- bootstrap_list(
  dt,
  dfidx = TRUE, shape = "wide", n = 1000, seed = 123,
  choice = "ch", varying = 1:4, ranked = TRUE
)
save(
  bootstrapped_mdat,
  file = here::here("output", "bootstrapped_mdat_1000.Rda")
)

mdat$ch # logical: TRUE if id2 was ranked idx1-th, by unit id1
mdat$id # respondent id
mdat$idx # position id (1st, 2nd, 3rd, 4th = depressed)
mdat$ideo7 # explanatory variable

# Run Rank-order logit model ===================================================
# Estimating parameters (no weight)
m <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + gender3 + age + race6 + educ + region,
  # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender" # Base category
)

# Estimating parameters (with weight)
m2 <- mlogit(
  ch ~ 1 | ideo7 + pid7 + partisan + gender3 + age + race6 + educ + region,
  # Y ~ X_item | X_resp
  mdat, # Data
  reflevel = "gender", # Base category
  weight = bias_weight
)

# Raw result
summary(m)
summary(m2)

# save mlogit object for checking
save(m, file = here::here("output", "full_raw.Rda"))
save(m2, file = here::here("output", "full_correct.Rda"))

# Bootstrapped version =========================================================
# Estimating parameters (no weight)
m_noweight_boot <- bootstrapped_mdat %>%
  imap(
    ~ tryCatch(
      {
        out <- mlogit(
          ## https://cran.r-project.org/web/packages/mlogit/vignettes/c2.formula.data.html
          ## If you exclude race, it works fine, so using race6 instead of race
          ch ~ 1 |
            ideo7 + pid7 + partisan + gender3 + age + race6 + educ + region,
          .x,
          reflevel = "gender"
        )
        return(out)
      },
      error = function(e) {
        ## If race is included instead, it gives the following error:
        ## Error: Lapack routine dgesv: system is exactly singular: U[31,31] = 0
        ## 1, 5, 12, 18, 19, 20, 23, 26, 28, 33, ...
        message("Error: ", e$message)
        message("The error occurred at the ", .y, "th iteration")
        return(NULL)
      }
    )
  )
save(m_noweight_boot, file = here::here("output", "m_noweight_boot_norace.Rda"))

# Estimating parameters (with weight)
m_weighted_boot <- bootstrapped_mdat %>%
  map(
    ~ mlogit(
      ch ~ 1 |
        ideo7 + pid7 + partisan + gender3 + age + race6 + educ + region,
      .x,
      reflevel = "gender",
      weight = bias_weight
    )
  )
save(m_weighted_boot, file = here::here("output", "m_weighted_boot_norace.Rda"))

# Compute predicted probabilities ==============================================
# Compute predicted probabilities for any combinations of the explanatory vars
# Testing
pp_noweight <- predict(
  m_noweight_boot[[1]],
  newdata = mdat,
  type = "prob"
)

