source(here::here("R", "utilities.R"))

# Data import ==================================================================
## Trial code
## https://ballotpedia.org/Municipal_elections_in_San_Francisco,_California_(2015)
raw_df <- read_csv(
  here("data", "raw", "CastVoteRecords_RCV", "CVR_SFMayor2015.csv")
) %>%
  clean_names() %>%
  select(-contains("write_in"))

## See unique values for each candidate
raw_df %>%
  select(-pref_voter_id, -precinct_id, -precinct) %>%
  map_dbl(~ length(unique(.x)))
assert_that(all(raw_df$precinct_id == raw_df$precinct))
raw_df <- raw_df %>% 
  select(-contains("precinct_id")) %>%
  select(precinct, everything()) %>%
  mutate(across(where(is.numeric), ~ as.character(.x)))

# Create "columns" of first-second-third places ================================
## (Is the ballot order straight order columns?)
## "item" means the ballot order: e.g., herrera came 1st place in ballot
ballot_order <- raw_df %>%
  select(-pref_voter_id, -precinct) %>%
  names() %>%
  as_tibble() %>%
  mutate(item = row_number()) %>%
  rename(name = value)

## "rank" means the actual rank that the voter submitted ---> ended up not using
rank_df <- raw_df %>%
  pivot_longer(
    cols = francisco_herrera:stuart_schuffman,
    values_to = "rank"
  ) %>%
  filter(!is.na(rank)) %>%
  left_join(., ballot_order) %>%
  select(-name)

raw_df[is.na(raw_df)] <- "M"
pattern_raw <- raw_df %>%
  unite(col = "rank", francisco_herrera:stuart_schuffman, sep = "")

## Delete those that do not logically make sense
## e.g., not specifying first place but writing second and third
## Also, for now, keep only fully ranked ballots
pattern_raw <- pattern_raw %>%
  filter(grepl("1", rank)) %>%
  filter(grepl("2", rank)) %>%
  filter(grepl("3", rank))

View(sort(table(pattern_raw$rank), decreasing = TRUE))

## If we "flatten" the ballot without accounting for six candidates, i.e.,
## the position of items or who the items are ...
pattern_raw %>%
  mutate(rank = gsub("M", "", rank)) %>%
  ggplot(., aes(x = rank, y = ..count.. / sum(..count..))) + 
  geom_bar()

