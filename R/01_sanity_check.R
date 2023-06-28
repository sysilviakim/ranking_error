source(here::here("R", "utilities.R"))

# Import test data =============================================================
fname <- "american_rankchoice_june2023_testdata_0622.csv"
test_link <- yougov_import(fname)$main

test_link %>%
  ## straight|diagonal
  filter(grepl("SK:", freeform)) %>%
  select(freeform, matches("anc_tate"))

#   freeform                                                anc_tate anc_tate_row_rnd anc_tate_recorded
# 1 SK: straightlining experiment                           231      312              123              
# 2 SK: diagonalizing but in the opposite direction         321      123              321              
# 3 SK: nonrandom ranking trial 1                           123      132              132              
# 4 SK: nonrandom ranking trial 2                           123      213              213              
# 5 SK: nonrandom ranking trial 3                           123      312              312              
# 6 SK: nonrandom ranking trial 4...?                       123      231              231              
# 7 SK: final test to see why repeat questions might be off 123      231              231         

# Assertions for formal testing ================================================
## Tate ------------------------------------------------------------------------
test_link %>%
  filter(grepl("SK: nonrandom|final test", freeform)) %>%
  .$anc_tate %>%
  {all(. == "123")} %>%
  assert_that()

test_link %>%
  filter(grepl("diagonal", freeform)) %>%
  .$anc_tate_recorded %>%
  {all(. == "321")} %>%
  assert_that()

test_link %>%
  filter(grepl("straight", freeform)) %>%
  .$anc_tate_recorded %>%
  {all(. == "123")} %>%
  assert_that()

## Identity --------------------------------------------------------------------
test_link %>%
  filter(grepl("SK: nonrandom|final test", freeform)) %>%
  .$anc_identity %>%
  {all(. == "1234")} %>%
  assert_that()

test_link %>%
  filter(grepl("diagonal", freeform)) %>%
  .$anc_identity_recorded %>%
  {all(. == "4321")} %>%
  assert_that()

test_link %>%
  filter(grepl("straight", freeform)) %>%
  .$anc_identity_recorded %>%
  {all(. == "1234")} %>%
  assert_that()

## Polarization ----------------------------------------------------------------
test_link %>%
  filter(grepl("SK: nonrandom|final test", freeform)) %>%
  .$anc_polar %>%
  {all(. == "12345")} %>%
  assert_that()

test_link %>%
  filter(grepl("diagonal", freeform)) %>%
  .$anc_polar_recorded %>%
  {all(. == "54321")} %>%
  assert_that()

test_link %>%
  filter(grepl("straight", freeform)) %>%
  .$anc_polar_recorded %>%
  {all(. == "12345")} %>%
  assert_that()

## Electoral systems -----------------------------------------------------------
test_link %>%
  filter(grepl("SK: nonrandom|final test", freeform)) %>%
  .$anc_esystems %>%
  {all(. == "123456")} %>%
  assert_that()

test_link %>%
  filter(grepl("diagonal", freeform)) %>%
  .$anc_esystems_recorded %>%
  {all(. == "654321")} %>%
  assert_that()

test_link %>%
  filter(grepl("straight", freeform)) %>%
  .$anc_esystems_recorded %>%
  {all(. == "123456")} %>%
  assert_that()

