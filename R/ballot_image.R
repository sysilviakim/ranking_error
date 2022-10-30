source(here::here("R", "utilities.R"))

# Data import ==================================================================
## Trial code
## https://ballotpedia.org/Municipal_elections_in_San_Francisco,_California_(2015)
df <- read_csv(
  here("data", "raw", "CastVoteRecords_RCV", "CVR_SFMayor2015.csv")
) %>%
  clean_names() %>%
  select(-contains("write_in"))

## See unique values for each candidate
df %>%
  select(-pref_voter_id, -precinct_id, -precinct) %>%
  map_dbl(~ length(unique(.x)))
assert_that(all(df$precinct_id == df$precinct))
df <- df %>% 
  select(-contains("precinct_id")) %>%
  select(precinct, everything()) %>%
  mutate(across(where(is.numeric), ~ as.character(.x)))

## Is the ballot order straight order columns?
ballot_order <- df %>%
  select(-pref_voter_id, -precinct) %>%
  names()

df[is.na(df)] <- "M" ## substitute missing value explicitly
