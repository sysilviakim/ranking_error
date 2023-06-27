source(here::here("R", "utilities.R"))

# Import data ==================================================================
fname <- "american_rankchoice_june2023_testdata_0622.csv"
test_link <- yougov_import(fname)$main

test_link %>%
  ## straight|diagonal
  filter(grepl("SK:", freeform)) %>%
  select(freeform, matches("anc_tate")) 

#   freeform                                                anc_tate anc_tate_row_rnd anc_tate_observed
#   <chr>                                                   <chr>    <chr>            <chr>            
# 1 SK: straightlining experiment                           231      312              123              
# 2 SK: diagonalizing but in the opposite direction         321      123              321              
# 3 SK: nonrandom ranking trial 1                           123      132              132              
# 4 SK: nonrandom ranking trial 2                           123      213              213              
# 5 SK: nonrandom ranking trial 3                           123      312              312              
# 6 SK: nonrandom ranking trial 4...?                       123      231              231              
# 7 SK: final test to see why repeat questions might be off 123      231              231         