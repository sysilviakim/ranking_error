source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03-straightlining-test.csv")
main <- df_list$main

# Setup ========================================================================
prep_list <- root_var %>%
  imap(
    ~ {
      dat <- main %>%
        ## For each application, delete partial rankings for both
        ## main and anchor variable. Need respondent to answer both fully.
        filter(
          !grepl("9", !!as.name(paste0("anc_", .y))) &
            !grepl("9", !!as.name(paste0("app_", .y)))
        ) %>%
        select(
          contains(paste0("app_", .y)),
          matches(paste0("anc_", .y)), # Keep original anchor items
          matches(paste0("anc_correct_", .y, "$")),
          matches(paste0("anc_", .y, "_observed"))
        ) %>%
        select(-contains("repeat"), -contains("trunc"))

      crosswalk <- option_crosswalk[option_crosswalk %in% names(dat)]
      dat <- dat %>% rename(!!!crosswalk)
      N <- nrow(dat)
      return(
        list(dat = dat, N = N, crosswalk = crosswalk, labels = names(crosswalk))
      )
    }
  )

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
ggsave(
  here("fig", "pretest", "pretest03-tate-main.pdf"),
  width = 4.5, height = 2.8
)

print(uniform_list$tate$anch)
ggsave(
  here("fig", "pretest", "pretest03-tate-anchor.pdf"),
  width = 4.5, height = 2.8
)

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

# Descriptive statistics========================================================
## Tate 1993 (representation) --------------------------------------------------

### Full visualization
vis_ranking(
  dat = prep_list$tate$dat,
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork")
)

### Test
vis_ranking(
  dat = prep_list$tate$dat,
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork"),
  treat = "anc_correct_tate"
)

### Compare substantial understanding
p1 <- vis_ranking(
  dat = prep_list$tate$dat,
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork"),
  single_plot = FALSE
) %>%
  .$p_avg

p2 <- vis_ranking(
  dat = prep_list$tate$dat %>% filter(anc_correct_tate == 1),
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork"),
  single_plot = FALSE
) %>%
  .$p_avg

p3 <- vis_ranking(
  dat = prep_list$tate$dat %>% filter(anc_correct_tate == 0),
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork"),
  single_plot = FALSE
) %>%
  .$p_avg

### Just a placeholder for future corrected figure
plot_nolegend(pdf_default(p2)) + ggtitle("")
ggsave(
  here("fig", "pretest", "tate_anchor_passed.pdf"),
  width = 3, height = 2.2
)

plot_nolegend(pdf_default(p3)) + ggtitle("")
ggsave(
  here("fig", "pretest", "tate_anchor_failed.pdf"),
  width = 3, height = 2.2
)




## Electoral systems -----------------------------------------------------------
dt_e_systems_w <- weight_patterns(
  dat = prep_list$e_systems$dat,
  main_q = "app_e_systems",
  anchor_q = "anc_e_systems",
  anc_correct = "anc_correct_e_systems",
  main_labels = prep_list$e_systems$labels,
  J = 7
)

head(dt_e_systems_w)
table(dt_e_systems_w$weight)

## Identity --------------------------------------------------------------------
vis_ranking(
  dat = prep_list$identity$dat,
  target_item = "party",
  other_items = setdiff(prep_list$identity$labels, "party")
)

ggsave(
  here("fig", "pretest", "pretest03-statistics-id-party.pdf"),
  width = 6, height = 4.5
)


## Polarization ----------------------------------------------------------------




# Improved Average Ranks =====================================================
imp_tate <- imprr(
  data = prep_list$tate$dat,
  rank_q = prep_list$tate$labels,
  target = "policy",
  anc_correct = "anc_correct_tate"
)

imp_tate_asy <- imprr(
  data = prep_list$tate$dat,
  rank_q = prep_list$tate$labels,
  target = "policy",
  anc_correct = "anc_correct_tate",
  asymptotics = TRUE
)

imp_tate
imp_tate_asy

imp_id <- imprr(
  data = prep_list$identity$dat,
  rank_q = prep_list$identity$labels,
  target = "party",
  anc_correct = "anc_correct_identity"
  )

imp_id_asy <- imprr(
  data = prep_list$identity$dat,
  rank_q = prep_list$identity$labels,
  target = "party",
  anc_correct = "anc_correct_identity",
  asymptotics = TRUE
  )

imp_id
imp_id_asy

imp_es <- imprr(
  data = prep_list$e_systems$dat,
  rank_q = prep_list$e_systems$labels,
  target = "minority",
  anc_correct = "anc_correct_e_systems"
)

imp_es_asy <- imprr(
  data = prep_list$e_systems$dat,
  rank_q = prep_list$e_systems$labels,
  target = "minority",
  anc_correct = "anc_correct_e_systems",
  asymptotics = TRUE

)

imp_es
imp_es_asy


imp_af <- imprr(
  data = prep_list$polar$dat,
  rank_q = prep_list$polar$labels,
  target = "media",
  anc_correct = "anc_correct_polar"
)

imp_af_asy <- imprr(
  data = prep_list$polar$dat,
  rank_q = prep_list$polar$labels,
  target = "media",
  anc_correct = "anc_correct_polar",
  asymptotics = TRUE
)


imp_af
imp_af_asy


# Visualization
imp_tate$topic <- "representation"
imp_id$topic <- "identity"
imp_es$topic <- "electoral systems"
imp_af$topic <- "polarization"
imp_tate_asy$topic <- "representation"
imp_id_asy$topic <- "identity"
imp_es_asy$topic <- "electoral systems"
imp_af_asy$topic <- "polarization"


p1 <- viz_avg(imp_tate) + 
  ggtitle("Representation") 
p2 <- viz_avg(imp_id) +
  ggtitle("Identity")
p3 <- viz_avg(imp_es) +
  ggtitle("Electoral Systems") 
p4 <- viz_avg(imp_af) + 
  ggtitle("Affective Polarization") 
p5 <- viz_avg(imp_tate_asy) + 
  ggtitle("Representation (asymptotics)") 
p6 <- viz_avg(imp_id_asy) +
  ggtitle("Identity  (asymptotics)")
p7 <- viz_avg(imp_es_asy) +
  ggtitle("Electoral Systems  (asymptotics)") 
p8 <- viz_avg(imp_af_asy) + 
  ggtitle("Affective Polarization  (asymptotics)") 


# m1 <- ggpubr::ggarrange(p1, p2, p3, p4, 
#                   nrow = 2, ncol = 2, 
#                   common.legend = TRUE,
#                   legend = "bottom")


# Just for now
m1 <- ggpubr::ggarrange(p1, p5, p2, p6, p3, p7, p4, p8,
                  nrow = 4, ncol = 2,
                  common.legend = TRUE,
                  legend = "bottom")


ggsave(here::here("fig/pretest", "pretest03-avg.pdf"),
       width = 6.5, height = 5*2)


