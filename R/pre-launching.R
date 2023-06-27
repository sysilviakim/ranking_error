source(here::here("R", "utilities.R"))

# Import data ==================================================================
test_link <-
  yougov_import("american_rankchoice_june2023_testdata_0622.csv")$main %>%
  filter(!is.na(religpew))
mock_data <- yougov_import("american_rankchoice_june2023.csv")$main

## To compare with Qualtrics
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# Check names ==================================================================
setdiff(names(mock_data), names(test_link))
#  [1] "identity"                "reverse"
#  [3] "tate_order"              "type"
#  [5] "no_context_3"            "no_context_4"
#  [7] "anc_tate"                "app_tate"
#  [9] "anc_identity"            "app_identity"
# [11] "alpha"                   "exact"
# [13] "anc_polar"               "app_polar"
# [15] "esystems"                "anc_app_esystems"
# [17] "anc_esystems_alphabet"   "anc_esystems_temporal"
# [19] "app_tate_repeat"         "app_identity_repeat"
# [21] "app_polar_repeat"        "anc_app_esystems_repeat"

View(mock_data[, setdiff(names(mock_data), names(test_link))])
# Because they are all filled with 9s, hard to know what these are

# Combine and investigate ======================================================
# df <- bind_rows(mock_data, test_link)
# Not advisable. immigrant in double for mock data, similarly for other vars
# e.g., age4, respondent_status, consent, gender3, race, ...
# These are all character in the test link data
# Not sure what the final output will resemble

test_link %>%
  select(contains("no_context_3"))
# A tibble: 9 × 8
#   no_context_3_1 no_context_3_2 no_context_3_3 m_no_context_3_module_rnd no_context_3_col_rnd no_context_3_row_rnd  p_no_context_3_page_rnd page_no_context_3_page_timing
#            <dbl>          <dbl>          <dbl> <chr>                     <chr>                <chr>                 <chr>                                           <dbl>
# 1              2              3              1 as-is(0, [0,1])           as-is(0, [0,1,2])    randomize(5, [2,0,1]) as-is(0, [0,1])                                 14.2
# 2              2              1              3 as-is(0, [0,1])           as-is(0, [0,1,2])    randomize(0, [0,1,2]) as-is(0, [0,1])                                 47.6
# 3             NA             NA             NA as-is(0, [0,1])           NA                   NA                    NA                                               0
# 4              3              2              1 as-is(0, [0,1])           as-is(0, [0,1,2])    randomize(0, [0,1,2]) as-is(0, [0,1])                                 11.0
# 5              2              3              1 as-is(0, [0,1])           as-is(0, [0,1,2])    randomize(5, [2,0,1]) as-is(0, [0,1])                                 31.2
# 6             NA             NA             NA as-is(0, [0,1])           NA                   NA                    NA                                               0
# 7              2              3              1 as-is(0, [0,1])           as-is(0, [0,1,2])    randomize(5, [2,0,1]) as-is(0, [0,1])                                  4.39
# 8             NA             NA             NA as-is(0, [0,1])           NA                   NA                    NA                                               0
# 9             NA             NA             NA as-is(0, [0,1])           NA                   NA                    NA                                               0

test_link %>%
  select(contains("app_tate")) %>%
  select(-contains("repeat"))

# A tibble: 9 × 7
#   app_tate_1 app_tate_2 app_tate_3 app_tate_col_rnd  app_tate_row_rnd      p_app_tate_page_rnd page_app_tate_page_timing
#        <dbl>      <dbl>      <dbl> <chr>             <chr>                 <chr>                                   <dbl>
# 1          1          2          3 as-is(0, [0,1,2]) randomize(1, [1,0,2]) as-is(0, [0,1])                          5.63
# 2          1          2          3 as-is(0, [0,1,2]) randomize(0, [0,1,2]) as-is(0, [0,1])                         23.0
# 3          1          3          2 as-is(0, [0,1,2]) randomize(2, [0,2,1]) as-is(0, [0,1])                         27.5
# 4          3          2          1 as-is(0, [0,1,2]) randomize(0, [0,1,2]) as-is(0, [0,1])                         20.8
# 5          1          2          3 as-is(0, [0,1,2]) randomize(4, [2,1,0]) as-is(0, [0,1])                         27.2
# 6          1          2          3 as-is(0, [0,1,2]) randomize(0, [0,1,2]) as-is(0, [0,1])                        223.
# 7          1          2          3 as-is(0, [0,1,2]) randomize(5, [2,0,1]) as-is(0, [0,1])                          9.33
# 8          1          2          3 as-is(0, [0,1,2]) randomize(2, [0,2,1]) as-is(0, [0,1])                        245.
# 9          1          2          3 as-is(0, [0,1,2]) randomize(1, [1,0,2]) as-is(0, [0,1])                         18.0

# Looks great
test_link %>%
  select(contains("row_rnd")) %>%
  View()

# Recovering diagonalizing + sincere tests
sk_test <- test_link %>%
  filter(birthyr == "1920") %>%
  filter(grepl("SK", freeform))

sk_test %>%
  select(contains("no_context_3"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing")
  ) %>%
  filter(!is.na(no_context_3_1))

#   no_context_3_1 no_context_3_2 no_context_3_3 no_context_3_row_rnd  freeform
#            <dbl>          <dbl>          <dbl> <chr>                 <chr>
# 1              3              2              1 randomize(0, [0,1,2]) SK: diagonalizing but in the opposite direction
# 2              2              3              1 randomize(5, [2,0,1]) SK: nonrandom ranking trial 1
# 3              2              3              1 randomize(5, [2,0,1]) SK: nonrandom ranking trial 3

# Yes, diagonal
main %>%
  # 312 is the equivalent for `randomize(5, [2,0,1])`
  filter(no_context_3_options_do == "312" & no_context_3_options == "231") %>%
  .$no_context_3_options_observed %>%
  table()

sk_test %>%
  select(contains("app_tate"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat")
  )

#   app_tate_1 app_tate_2 app_tate_3 app_tate_row_rnd      freeform                                               
#        <dbl>      <dbl>      <dbl> <chr>                 <chr>                                                  
# 1          1          3          2 randomize(2, [0,2,1]) SK: straightlining experiment                          
# 2          3          2          1 randomize(0, [0,1,2]) SK: diagonalizing but in the opposite direction        
# 3          1          2          3 randomize(4, [2,1,0]) SK: nonrandom ranking trial 1                          
# 4          1          2          3 randomize(0, [0,1,2]) SK: nonrandom ranking trial 2                          
# 5          1          2          3 randomize(5, [2,0,1]) SK: nonrandom ranking trial 3                          
# 6          1          2          3 randomize(2, [0,2,1]) SK: nonrandom ranking trial 4...?                      
# 7          1          2          3 randomize(1, [1,0,2]) SK: final test to see why repeat questions might be off

sk_test %>%
  select(contains("anc_tate"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat")
  )

#   anc_tate_1 anc_tate_2 anc_tate_3 anc_tate_row_rnd      freeform                                               
#        <dbl>      <dbl>      <dbl> <chr>                 <chr>                                                  
# 1          2          3          1 randomize(5, [2,0,1]) SK: straightlining experiment                          
# 2          3          2          1 randomize(0, [0,1,2]) SK: diagonalizing but in the opposite direction        
# 3          1          2          3 randomize(2, [0,2,1]) SK: nonrandom ranking trial 1                          
# 4          1          2          3 randomize(1, [1,0,2]) SK: nonrandom ranking trial 2                          
# 5          1          2          3 randomize(5, [2,0,1]) SK: nonrandom ranking trial 3                          
# 6          1          2          3 randomize(3, [1,2,0]) SK: nonrandom ranking trial 4...?                      
# 7          1          2          3 randomize(3, [1,2,0]) SK: final test to see why repeat questions might be off

sk_test %>%
  select(contains("anc_identity"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat"),
    -contains("alphabet"), -contains("exact")
  )

sk_test %>%
  select(contains("anc_identity"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat")
  ) %>%
  select(contains("alphabet"), freeform) %>%
  filter(!is.na(anc_identity_alphabet_1))

# Where did I mess up as a researcher and generate 1-2-4-3?
# Oh no, it's working as intended, because in YouGov survey, reference set was
# exact_1- prompt once on skip Friends
# exact_2- prompt once on skip Parents
# exact_3- prompt once on skip Relatives
# exact_4- prompt once on skip Teachers

sk_test %>%
  select(contains("anc_identity"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat")
  ) %>%
  select(contains("exact"), freeform) %>%
  filter(!is.na(anc_identity_exact_1))

sk_test %>%
  select(contains("anc_polar"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat"),
    -contains("alphabet"), -contains("temporal")
  )

sk_test %>%
  select(contains("anc_esystems"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat"),
    -contains("alphabet"), -contains("temporal")
  )

sk_test %>%
  select(contains("anc_esystems"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat")
  ) %>%
  select(contains("alphabet"), freeform) %>%
  filter(!is.na(anc_esystems_alphabet_1))

sk_test %>%
  select(contains("anc_esystems"), freeform) %>%
  select(
    -contains("col_rnd"), -contains("page_rnd"),
    -contains("module_rnd"), -contains("timing"), -contains("repeat")
  ) %>%
  select(contains("temporal"), freeform) %>%
  filter(!is.na(anc_esystems_temporal_1))

# Other diagonalizing of anchor questions all pass!
