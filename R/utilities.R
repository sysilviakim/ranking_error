# Library and function calls ===================================================
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
