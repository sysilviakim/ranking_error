source(here::here("R/affective_polarization", "01_setup.R"))
data <- df_list$main %>%
  select(
    app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5,
    anc_polar_1, anc_polar_2, anc_polar_3, anc_polar_4, anc_polar_5,
    anc_correct_polar
  )

# Bias-correction (direct + IPW) ===============================================
fname <- here("output", "polar.Rda")
if (!file.exists(fname)) {
  direct_out <- imprr_direct(
    data = data,
    J = 5,
    main_q = "app_polar",
    anchor_q = "anc_polar",
    anc_correct = "anc_correct_polar",
    n_bootstrap = 1000
  )
  
  ipw_out <- imprr_weights(
    data = data,
    J = 5,
    main_q = "app_polar",
    anchor_q = "anc_polar",
    anc_correct = "anc_correct_polar",
    n_bootstrap = 1000
  )

  save(list = c("direct_out", "ipw_out"), file = fname)
} else {
  load(fname)
}


## Join computed weights to main data
dt_w <- main %>%
  select(
    app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5,
    app_polar, everything()
  ) %>%
  left_join(ipw_out$weights, by = c("app_polar" = "ranking"))

# Average rank calculations ====================================================
## By subgroups ----------------------------------------------------------------
## All sample: raw and corrected
avg_rank(main, "app_polar", items = item_list$item, round = 2)
avg_rank(
  dt_w %>%
    select(app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5, w), 
  raw = FALSE, weight = "w", items = item_list, round = 2
)
# direct_out$qoi %>%
#   filter(qoi == "average rank") %>%
#   select(mean, lower, upper) %>%
#   mutate(method = "Direct")

## By partisanship: raw
summ_avg_rank_by_group(
  main,
  v = "pid3final", items = item_list$item, label = "Partisanship"
)

## By partisanship: raw
summ_avg_rank_by_group(
  dt_w,
  v = "pid3final", items = item_list, label = "Partisanship",
  raw = FALSE, weight = "w"
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
