# Library and function calls ===================================================
library(plyr)
library(MASS)
library(here)
library(combinat) # For permn
library(gtools) # For permute
library(tidyverse)
library(styler)
library(PLMIX) # For switch_ord_rank()

# READING THE FUNCTION THAT DRAW FROM PRACKETT-LUCE
source(here("R", "rpluce.R"))

# Temporary functions ==========================================================
pivot_sim <- function(x) {
  out <- x %>%
    pivot_longer(
      cols = c(V1, V2, V3), names_to = "position", values_to = "item"
    ) %>%
    mutate(
      rank = case_when(
        position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
        position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
        position == "V3" ~ str_sub(obs_rank, 3, 3)
      )
    ) %>% # Third value
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
  
  for (i in 1:dim(true_rank)[1]) { # For each i-th unit in true_rank
    
    # Cast numbers to alphabets
    vec_pref <- true_rank[i, ] %>%
      pivot_longer(cols = contains("Item"), names_to = "variable") %>%
      mutate(
        variable = case_when(
          variable == "Item_1" ~ "a",
          variable == "Item_2" ~ "b",
          variable == "Item_3" ~ "c"
        )
      )
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
  return(obs_pattern)
}

prop_vector <- function(x, digits = 1) {
  return(round(prop.table(table(x, useNA = "ifany")), digits = digits))
}
