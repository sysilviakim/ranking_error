source(here::here("R", "utilities.R"))

# Import and clean in list =====================================================
p_list <- list.files(
  here("data", "raw", "CastVoteRecords_RCV"),
  pattern = ".csv", full.names = TRUE
) %>%
  set_names(
    ., gsub("\\.csv|/", "", gsub(here("data/raw/CastVoteRecords_RCV"), "", .))
  ) %>%
  imap(
    ~ {
      ## Read raw data
      raw_df <- read_csv(.x) %>%
        clean_names() %>%
        select(-contains("write_in"))

      if (grepl("SFMayor2007|SFMayor2018", .y)) {
        ## Some voters, such as voter ID 12900 for SFMayor2007,
        ## gave all three options for one candidate
        ## discard such invalid ballots
        raw_df <- raw_df %>%
          group_by(voter_id, contest_id, candidate_id) %>%
          filter(!(n() > 1))

        ## This was provided in a long data format as opposed to the usual wide
        ## Turn into wide
        raw_df <- raw_df %>%
          group_by(voter_id) %>%
          pivot_wider(names_from = candidate_id, values_from = vote_rank) %>%
          ungroup()

        raw_df <- raw_df[, order(names(raw_df))] %>%
          select(contest_id, id = voter_id, everything()) %>%
          select(-contains("serial"), -contains("tally")) %>%
          ## It looks like `0` candidate ID might be ... missing or write-in
          ## Cross-check later
          select(-`0`)

        ## I have absolutely no idea who these candidates are: 178, 179, ... 185
      }

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
        mutate(across(where(is.numeric), ~ as.character(.x))) %>%
        mutate(across(where(is.logical), ~ as.character(.x)))

      ## Create ballot order crosswalk
      ## (Is the ballot order straight order columns?)
      ## "item" means the ballot order: e.g., herrera came 1st place in ballot
      ballot_order <- raw_df %>%
        select(-matches("^id$|_id$"), -contains("precinct")) %>%
        names() %>%
        as_tibble() %>%
        mutate(item = row_number()) %>%
        rename(name = value)

      ## Code to substitute missing values
      raw_df[is.na(raw_df)] <- "M"

      ## Unite into a typical pattern of 123, 312, ...
      pattern_raw <- raw_df %>%
        select(-id, -contains("precinct"), -matches("^contest")) %>%
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
        ylab("") +
        ggtitle(gsub("CVR_", "", gsub("Mayor", " ", .y)))

      print(paste0("Successful launch for ", .y))
      return(p)
    }
  )

# Use patchworks to wrap the figures into a single figure ======================
p <- p_list %>%
  map(
    ~ pdf_default(.x) +
      scale_y_continuous(limits = c(0, 0.415)) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 10)
      )
  ) %>%
  wrap_plots(ncol = 4)

pdf(here("fig", "mayoral_race_rankings_collapsed.pdf"), width = 7, height = 5)
print(p)
dev.off()
