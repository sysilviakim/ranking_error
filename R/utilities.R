# Library and function calls ===================================================
library(plyr)
library(MASS)
library(here)
library(combinat) # For permn
library(gtools) # For permute
library(tidyverse)
library(styler)
library(PLMIX) # For switch_ord_rank()
library(Kmisc)
library(patchwork)
library(janitor)
library(assertthat)

# READING THE FUNCTION THAT DRAW FROM PLACKETT-LUCE
source(here("R", "rpluce.R"))

# Functions ====================================================================
## Turn 1st, 2nd, and 3rd ranking columns into long data format
pivot_sim <- function(x) {
  out <- x %>%
    pivot_longer(
      cols = c(V1, V2, V3), names_to = "position", values_to = "item"
    ) %>%
    mutate(
      rank = case_when(
        position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
        position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
        position == "V3" ~ str_sub(obs_rank, 3, 3) # Third value
      )
    ) %>%
    arrange(id, rank)
  return(out)
}

loop_stated_rank_preference <- function(true_rank, choice) {
  # Storage for observed patterns
  obs_pattern <- data.frame(
    pattern1 = integer(),
    pattern2 = integer(),
    pattern3 = integer()
  )

  # For each i-th unit in true_rank
  for (i in 1:dim(true_rank)[1]) {
    # Cast numbers to alphabets
    vec_pref <- true_rank[i, ] %>%
      pivot_longer(cols = c(a, b, c), names_to = "variable")
    vec_pref # Check

    # Alphabet unit i sees in each position
    position1 <- choice[i, 1] %>% pull()
    position2 <- choice[i, 2] %>% pull()
    position3 <- choice[i, 3] %>% pull()

    # Assign a value (rank) for each position
    # given the alphabet unit i sees there
    pattern1 <- vec_pref$value[vec_pref$variable == position1]
    pattern2 <- vec_pref$value[vec_pref$variable == position2]
    pattern3 <- vec_pref$value[vec_pref$variable == position3]

    # Combine the result
    comb <- data.frame(
      pattern1 = pattern1,
      pattern2 = pattern2,
      pattern3 = pattern3
    )

    # Stack in the storage
    obs_pattern <- rbind(obs_pattern, comb)

    # End of i loop
  }

  obs_pattern <- obs_pattern %>% unite(obs_rank, sep = "")
  return(as_tibble(obs_pattern))
}

prop_vector <- function(x, digits = 1) {
  return(round(prop.table(table(x, useNA = "ifany")), digits = digits))
}

rowid <- function(x) {
  out <- x %>%
    mutate(id = row_number()) %>%
    select(id, everything())
  return(out)
}

pivot_join <- function(x, y) {
  return(pivot_sim(left_join(y, x)) %>% select(id, obs_rank, item, rank))
}

# Added on 2/6/2023
# This function recovers the reference (true) ranking
# with respect to the reference item set (here: {abc})
recov_ref_ranking_new <- function(dat) {
  # For each i-th unit in data
  ref_data <- seq(nrow(dat)) %>%
    map(
      ~ {
        temp <- dat[.x, ] %>%
          separate(obs, sep = c(1, 2), into = c("first", "second", "third")) %>%
          ## V1, V2, and V3 are randomized choice order given to respondent
          select(first, second, third, contains("V")) %>%
          pivot_longer(cols = c(V1, V2, V3), names_to = "variable")
        
        ## Recovering the true ranking given the reference set (abc)
        ## case_when is faster outside a pipe
        temp$recover <- case_when(
          temp$variable == "V1" ~ temp$first,
          temp$variable == "V2" ~ temp$second,
          temp$variable == "V3" ~ temp$third
        )
        
        temp <- temp %>%
          arrange(value) %>%
          .$recover %>%
          ## ref_ranking, i.e., concatenated true rankings
          paste(collapse = "")
        return(temp)
      }
    ) %>%
    ## combine
    unlist() %>%
    tibble(ref_data = .)
  return(ref_data)
}
