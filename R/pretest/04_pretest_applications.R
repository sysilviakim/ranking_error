source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03-straightlining-test.csv")
main <- df_list$main

# Wrangle data before splitting ================================================
main <- main %>%
  mutate(
    white = case_when(
      race == "1" ~ "White",
      TRUE ~ "Non-white"
    ),
    turnout2022 = case_when(
      turnout2022 == "1" ~ "Yes",
      TRUE ~ "No"
    ),
    college = case_when(
      education %in% c("1", "2", "3") ~ "Less Than BA",
      TRUE ~ "Bachelor's Degree or Higher"
    )
  )

# Setup ========================================================================
prep_list <- root_var %>%
  imap(
    ~ {
      ## For each application, delete partial rankings for both
      ## main and anchor variable. Need respondent to answer both fully.

      dat <- main %>%
        filter(
          !grepl("9", !!as.name(paste0("anc_", .y))) &
            !grepl("9", !!as.name(paste0("app_", .y)))
        )
      N <- nrow(dat)

      ## Indices
      set.seed(123)
      indices <- seq(bootstrap_n) %>%
        map(
          function(y) {
            tibble(
              !!as.name(paste0("x", y)) :=
                sample(seq(N), size = N, replace = TRUE)
            )
          }
        ) %>%
        bind_cols()
      assert_that(!identical(sort(indices$x1), sort(indices$x2)))

      ## Correct answers to the anchor question
      correct <- ifelse(dat[[paste0("anc_", .y)]] == .x, 1, 0)

      return(
        list(
          dat = dat, N = N, indices = indices, correct = correct, answer = .x
        )
      )
    }
  )

## Sanity check
assert_that(nchar(prep_list$tate$answer) == 3)

# Uniform distribution test ====================================================
## Table prep ------------------------------------------------------------------
uniform_list <- prep_list %>%
  imap(
    ~ {
      ### Create frequency tables
      tab_main <- table(.x$dat[[paste0("app_", .y, "_observed")]])
      tab_anch <- table(.x$dat[[paste0("anc_", .y, "_observed")]])

      ### Chi-square test and power test
      message(paste0("Chi-square test result for main question, ", .y))
      chisq_power(tab_main)

      message(paste0("Chi-square test result for anchor question, ", .y))
      chisq_power(tab_anch)

      temp <- table_to_tibble(tab_main)
      suppressMessages(({
        p_main <- plot_nolegend(pdf_default(plot_dist_ranking(temp)))
      }))

      temp <- table_to_tibble(tab_anch)
      suppressMessages(({
        p_anch <- plot_nolegend(pdf_default(plot_dist_ranking(temp)))
      }))

      return(list(main = p_main, anch = p_anch))
    }
  )

## Visualization ---------------------------------------------------------------
## For other Qs, not much point in visualizing
print(uniform_list$tate$main)
ggsave(here("fig", "pretest03-tate-main.pdf"), width = 4.5, height = 2.8)

print(uniform_list$tate$anch)
ggsave(here("fig", "pretest03-tate-anchor.pdf"), width = 4.5, height = 2.8)

# Average rankings =============================================================
## Tate 1993 (representation) --------------------------------------------------

### All responses
avg_rank(main %>% filter(!grepl("9", app_tate)), "app_tate")
# (1) Working in Congress on bills concerning national issues ---> 1.84
# (2) Making sure the state/district gets its fair share of government money and projects ---> 1.91
# (3) Helping people in the district who have personal problems with government ---> 2.26

### Non-geometric
avg_rank(
  main %>% filter(!grepl("9", app_tate)) %>% filter(ns_tate == 0),
  "app_tate"
)

# (1) Working in Congress on bills concerning national issues ---> 1.63
# (2) Making sure the state/district gets its fair share of government money and projects ---> 1.96
# (3) Helping people in the district who have personal problems with government ---> 2.41

## Electoral systems -----------------------------------------------------------
### All responses
avg_rank(main %>% filter(!grepl("9", app_e_systems)), "app_e_systems")

# (1) Hold each politician accountable for their actions ---> 3.74
# (2) Hold the government as a whole accountable for its actions ---> 3.69
# (3) Make sure that the government is stable ---> 3.42
# (4) Give seats to parties in proportion to their voter support ---> 4.58
# (5) Women will be fairly represented ---> 4.29
# (6) People of color will be fairly represented ---> 4.47
# (7) Policies match with what the average voter wants ---> 3.79

### Non-geometric
avg_rank(
  main %>% filter(!grepl("9", app_e_systems)) %>% filter(ns_e_systems == 0),
  "app_e_systems"
)

# (1) Hold each politician accountable for their actions ---> 3.27
# (2) Hold the government as a whole accountable for its actions ---> 4.2
# (3) Make sure that the government is stable ---> 4.0
# (4) Give seats to parties in proportion to their voter support ---> 4.47
# (5) Women will be fairly represented ---> 4.33
# (6) People of color will be fairly represented ---> 4.2
# (7) Policies match with what the average voter wants ---> 3.53

## Identity --------------------------------------------------------------------
### All responses
avg_rank(main %>% filter(!grepl("9", app_identity)), "app_identity")

# (1) Your political party ---> 4.96
# (2) Your job or occupation ---> 4.12
# (3) Your religion ---> 4.10
# (4) Your gender ---> 3.77
# (5) Your role in your family ---> 2.95
# (6) Being [INSERT RACE] ---> 4.47
# (7) Being American ---> 3.63

### Non-geometric
avg_rank(
  main %>% filter(!grepl("9", app_identity)) %>% filter(ns_identity == 0),
  "app_identity"
)

# (1) Your political party ---> 4.81
# (2) Your job or occupation ---> 4.44
# (3) Your religion ---> 3.81
# (4) Your gender ---> 3.70
# (5) Your role in your family ---> 2.93
# (6) Being [INSERT RACE] ---> 4.93
# (7) Being American ---> 3.37

## Polarization ----------------------------------------------------------------
### All responses
avg_rank(main %>% filter(!grepl("9", app_polar)), "app_polar")

# (1) Democratic politicians ---> 4.69
# (2) Republican politicians ---> 4.28
# (3) Print media and TV ---> 3.61
# (4) Social media ---> 4.02
# (5) Interest groups ---> 4.41
# (6) Democratic citizens ---> 4.94
# (7) Republican citizens ---> 4.84
# (8) Electoral systems ---> 5.2

### Non-geometric
avg_rank(
  main %>% filter(!grepl("9", app_polar)) %>% filter(ns_polar == 0),
  "app_polar"
)

# (1) Democratic politicians ---> 4.49
# (2) Republican politicians ---> 4.14
# (3) Print media and TV ---> 3.29
# (4) Social media ---> 3.69
# (5) Interest groups ---> 4.39
# (6) Democratic citizens ---> 5.49
# (7) Republican citizens ---> 4.86
# (8) Electoral systems ---> 5.65

# Descriptive Statistics========================================================
## Tate 1993 (representation) --------------------------------------------------

dt_id <- main %>%
  filter(!grepl("9", app_identity)) %>%
  mutate(anc_correct = ifelse(anc_identity == "1234567", 1, 0)) %>%
  select(
    app_identity_1:app_identity_7,
    app_identity, anc_identity, anc_correct
  ) %>%
  rename(
    party = app_identity_1,
    job = app_identity_2,
    religion = app_identity_3,
    gender = app_identity_4,
    family_role = app_identity_5,
    race = app_identity_6,
    American = app_identity_7
  )

others <- c(
  "job", "religion", "gender",
  "family_role", "race", "American"
)

vis_r(
  data = dt_id,
  target_item = "party", # Political party
  other_items = others
)

ggsave(
  here::here("fig", "pretest03-statistics-id-party.pdf"),
  width = 6, height = 4.5
)


# Compute weights

dt_id_w <- imprr(
  data = dt_id,
  rank_q = c(
    "party", "job", "religion", "gender",
    "family_role", "race", "American"
  ),
  main_q = "app_identity",
  anchor = "anc_identity",
  anc_correct = "anc_correct",
  J = 7
)

head(dt_id_w)

head(dt_id_w$weight)
table(dt_id_w$weight)

# [1] 0.01063827 0.01063827 0.01063827 0.01063827 0.01063827
# [6] 0.01063827
# --> This seems shady. Needs more checks

## Tate 1993 (representation) --------------------------------------------------
dt_rep <- main %>%
  filter(!grepl("9", app_tate)) %>%
  mutate(anc_correct = ifelse(anc_tate == "123", 1, 0)) %>%
  select(
    app_tate_1:app_tate_3,
    app_tate, anc_tate, anc_correct
  ) %>%
  rename(
    policy = app_tate_1,
    pork = app_tate_2,
    service = app_tate_3
  )

dt_rep_w <- imprr(
  data = dt_rep,
  rank_q = c("policy", "pork", "service"),
  main_q = "app_tate",
  anchor = "anc_tate",
  anc_correct = "anc_correct",
  J = 3
)

head(dt_rep_w) # --> Looks good!
table(
  dt_rep_w$weight,
  dt_rep_w$app_tate
)

# Unit check -- bias pulls the PMF to uniform distribution

N <- dim(dt_rep_w)[1]
w <- unique(dt_rep_w$weight)

freq_raw <- round(table(dt_rep_w$app_tate) / N, d = 2)
freq_imp <- round(table(dt_rep_w$app_tate) * w / N, d = 2)

# Raw frequency
freq_raw_dev <- round(table(dt_rep_w$app_tate) / N - 1 / 6, d = 2)

# Improved frequency
freq_imp_dev <- round(table(dt_rep_w$app_tate) * w / N - 1 / 6, d = 2)

freq_raw
freq_imp

mean(freq_raw_dev)
mean(freq_imp_dev)


## Electoral sys ---------------------------------------------------------------
dt_es <- main %>%
  filter(!grepl("9", app_e_systems)) %>%
  mutate(anc_correct = ifelse(anc_e_systems == "1234567", 1, 0)) %>%
  select(
    app_e_systems_1:app_e_systems_7,
    app_e_systems, anc_e_systems, anc_correct
  ) %>%
  rename(
    account_pol = app_e_systems_1,
    account_gov = app_e_systems_2,
    stable = app_e_systems_3,
    prop = app_e_systems_4,
    women = app_e_systems_5,
    minority = app_e_systems_6,
    median = app_e_systems_7
  )

dt_es_w <- imprr(
  data = dt_es,
  rank_q = c(
    "account_pol", "account_gov", "stable",
    "prop", "women", "minority", "median"
  ),
  main_q = "app_e_systems",
  anchor = "anc_e_systems",
  anc_correct = "anc_correct",
  J = 7
)

head(dt_es_w) # --> Looks NOT good
table(dt_es_w$weight)

# Proportion of non-random answers
# Representation (J=3)
# 0.4244898
mean(dt_rep_w$p_non_random)

# Identity (J=7)
# 0.2753664
mean(dt_id_w$p_non_random)

# Electoral systems (J=8)
# 0.1921474
mean(dt_es_w$p_non_random)
