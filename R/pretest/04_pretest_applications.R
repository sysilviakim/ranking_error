source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
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
      
      ## Recover the "observed" ranking
      ## based on randomized order presentation
      dat[[paste0("anc_", .y, "_obs")]] <- dat %>%
        separate(
          !!as.name(paste0("anc_", .y, "_do")),
          sep = seq(nchar(.x) - 1), into = paste0("V", seq(nchar(.x)))
        ) %>%
        recov_ref_ranking(rank_var = paste0("anc_", .y)) %>%
        .$ref
      
      dat[[paste0("app_", .y, "_obs")]] <- dat %>%
        separate(
          !!as.name(paste0("app_", .y, "_do")),
          sep = seq(nchar(.x) - 1), into = paste0("V", seq(nchar(.x)))
        ) %>%
        recov_ref_ranking(rank_var = paste0("app_", .y)) %>%
        .$ref
      
      return(
        list(
          dat = dat, N = N, indices = indices, correct = correct, answer = .x
        )
      )
    }
  )

## Sanity check
assert_that(nchar(prep_list$tate_1993$answer) == 3)

# Uniform distribution test ====================================================
## Table prep ------------------------------------------------------------------
uniform_list <- prep_list %>%
  imap(
    ~ {
      ### Create frequency tables
      tab_main <- table(.x$dat[[paste0("app_", .y, "_obs")]])
      tab_anch <- table(.x$dat[[paste0("anc_", .y, "_obs")]])
      
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
print(uniform_list$tate_1993$main)
ggsave(here("fig", "pretest03-tate_1993-main.pdf"), width = 4.5, height = 2.8)

print(uniform_list$tate_1993$anch)
ggsave(here("fig", "pretest03-tate_1993-anchor.pdf"), width = 4.5, height = 2.8)

# Average rankings =============================================================
## Tate 1993 (representation) --------------------------------------------------

### All responses
avg_rank(main %>% filter(!grepl("9", app_tate_1993)), "app_tate_1993")
# (1) Working in Congress on bills concerning national issues ---> 1.84
# (2) Making sure the state/district gets its fair share of government money and projects ---> 1.91
# (3) Helping people in the district who have personal problems with government ---> 2.26

### Non-geometric
avg_rank(
  main %>% filter(!grepl("9", app_tate_1993)) %>% filter(ns_tate == 0),
  "app_tate_1993"
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
  main %>% filter(!grepl("9", app_e_systems)) %>% filter(ns_esystem == 0),
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
