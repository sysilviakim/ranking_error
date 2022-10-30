source(here::here("R", "utilities.R"))

# Import and clean in list =====================================================
p_list <- list.files(
  here("data", "raw", "CastVoteRecords_RCV"),
  pattern = ".csv", full.names = TRUE
) %>%
  setdiff(
    ., c(
      here("data/raw/CastVoteRecords_RCV", "CVR_SFMayor2007.csv"),
      here("data/raw/CastVoteRecords_RCV", "CVR_SFMayor2018.csv")
    )
  ) %>%
  set_names(
    ., gsub("\\.csv|/", "", gsub(here("data/raw/CastVoteRecords_RCV"), "", .))
  ) %>%
  imap(
    ~ {
      raw_df <- read_csv(.x) %>%
        clean_names() %>%
        select(-contains("write_in"))
      
      if ("pref_voter_id" %in% names(raw_df)) {
        raw_df <- raw_df %>%
          rename(id = pref_voter_id)
      }
      
      ## See unique values for each candidate
      raw_df %>%
        select(-contains("id"), -contains("precinct")) %>%
        map_dbl(~ length(unique(.x)))
      
      ## Not necessarily true for some files; was true for SF 2018 mayoral
      ## assert_that(all(raw_df$precinct_id == raw_df$precinct))
      
      ## Turn numbers to characters after deleting repeated columns
      raw_df <- raw_df %>%
        select(-contains("precinct")) %>%
        mutate(across(where(is.numeric), ~ as.character(.x)))
      
      ## Create ballot order crosswalk
      ## (Is the ballot order straight order columns?)
      ## "item" means the ballot order: e.g., herrera came 1st place in ballot
      ballot_order <- raw_df %>%
        select(-id, -contains("precinct")) %>%
        names() %>%
        as_tibble() %>%
        mutate(item = row_number()) %>%
        rename(name = value)
      
      ## Code to substitute missing values
      raw_df[is.na(raw_df)] <- "M"
    
      ## Unite into a typical pattern of 123, 312, ...
      pattern_raw <- raw_df %>%
        select(-id, -contains("precinct")) %>%
        unite(col = "rank", sep = "")
      
      ## Delete those that do not logically make sense
      ## e.g., not specifying first place but writing second and third
      ## Also, for now, keep only fully ranked ballots
      pattern_raw <- pattern_raw %>%
        filter(grepl("1", rank)) %>%
        filter(grepl("2", rank)) %>%
        filter(grepl("3", rank))
      ## View(sort(table(pattern_raw$rank), decreasing = TRUE))
      
      ## If we "flatten" the ballot without accounting for six candidates, i.e.,
      ## the position of items or who the items are ...
      p <- pattern_raw %>%
        mutate(rank = gsub("M", "", rank)) %>%
        ggplot(., aes(x = rank, y = ..count.. / sum(..count..))) +
        geom_bar() + 
        xlab("") + 
        ylab("")
      
      print(paste0("Successful launch for ", .y))
      return(p)
    }
  )





