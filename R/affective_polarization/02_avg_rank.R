source(here::here("R/affective_polarization", "01_setup.R"))
data <- df_list$main %>%
  select(
    app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5,
    anc_polar_1, anc_polar_2, anc_polar_3, anc_polar_4, anc_polar_5,
    anc_correct_polar
  )

# Bias-correction (direct + IPW) ===============================================
direct_out <- imprr_direct(
  data = main,
  main_q = "app_polar",
  anchor_q = "anc_polar",
  anc_correct = "anc_correct_polar",
  n_bootstrap = 500
)

ipw_out <- imprr_weights(
  data = main,
  main_q = "app_polar",
  anchor_q = "anc_polar",
  anc_correct = "anc_correct_polar",
  n_bootstrap = 500
)

# Average rank calculations ====================================================
## By subgroups ----------------------------------------------------------------
## All sample
avg_rank(main, "app_polar", items = item_list$item)
avg_rank(dt_w, raw = FALSE, weight = "w", items = item_list, round = 2)

## By partisanship: raw
summ_avg_rank_by_group(
  main,
  v = "pid3final", items = item_list$item, label = "Partisanship"
)

## By race: raw
summ_avg_rank_by_group(
  main,
  v = "race4labeled", items = item_list$item, label = "Race"
)

## By political interest: raw
summ_avg_rank_by_group(
  main,
  v = "newsint_labeled", items = item_list$item,
  label = "Follow Politics (Political Interest)"
)



## Export via xtable -----------------------------------------------------------
tab <- xtable(summ_avg_rank_by_group(), digits = 2)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks.tex")
)

tab <- xtable(
  summ_avg_rank_by_group("pid3final", "Partisanship") %>%
    mutate(
      Partisanship = factor(
        Partisanship,
        levels = c("Democrat", "Republican", "Independent")
      )
    ),
  digits = 2
)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks_by_partisanship.tex")
)

tab <- xtable(
  summ_avg_rank_by_group("race4labeled", "Race") %>%
    mutate(
      Race = factor(
        Race,
        levels = c("White", "Black", "Hispanic", "Other race"),
        labels = c("White", "Black", "Hispanic/Latino", "Other race")
      )
    ),
  digits = 2
)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks_by_race.tex")
)

tab <- xtable(
  summ_avg_rank_by_group(
    "newsint_labeled", 
    "Follow Politics (Political Interest)"
  ) %>%
    mutate(
      `Follow Politics (Political Interest)` = factor(
        `Follow Politics (Political Interest)`,
        levels = c(
          "Most of the time", "Some of the time",
          "Only now and then", "Hardly at all"
        )
      )
    ),
  digits = 2
)
print(
  tab, include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "affpolar_avg_ranks_by_political_interest.tex")
)
