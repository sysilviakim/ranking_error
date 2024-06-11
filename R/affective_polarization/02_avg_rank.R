source(here::here("R/affective_polarization", "01_setup.R"))
library(weights)

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

# Proportion of prevalent rankings =============================================
## Later, export this via xtable and figure out the automation
sort(table(main$app_polar), decreasing = TRUE)
prop(main, "app_polar")

## Top 10?
count(dt_w, app_polar, wt = w) %>%
  mutate(prop = formatC(n / sum(n) * 100, digits = 2, format = "f")) %>%
  arrange(desc(prop)) %>%
  select(app_polar, prop) %>%
  slice_head(n = 10)

## And how much do they account for?
count(dt_w, app_polar, wt = w) %>%
  mutate(sum = sum(n)) %>%
  slice_head(n = 10) %>%
  mutate(top_sum = sum(n)) %>%
  mutate(top_perc = round(top_sum / sum * 100, digits = 1))

# Average rank calculations and visualization ==================================
## All sample: raw and corrected -----------------------------------------------
avg_rank(main, "app_polar", items = item_list$item, round = 2)
avg_rank(dt_w, raw = FALSE, weight = "w", items = item_list, round = 2)
# direct_out$qoi %>%
#   filter(qoi == "average rank") %>%
#   select(mean, lower, upper) %>%
#   mutate(method = "Direct")

df <- rbind(
  avg_rank(main, "app_polar", items = item_list$item, round = 2),
  avg_rank(
    dt_w %>%
      select(
        app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5, w
      ), 
    raw = FALSE, weight = "w", items = item_list, round = 2
  )
) %>%
  mutate(item = factor(item, levels = rev(item_list$item)))

p <- viz_avg_rank_temp(df)
pdf_default(p)
ggsave(here("fig", "avg_rank_polar.pdf"), width = 6, height = 2.5)

## By partisanship -------------------------------------------------------------
summ_avg_rank_by_group(
  main,
  v = "pid3final", items = item_list$item, label = "Partisanship"
)
summ_avg_rank_by_group(
  dt_w,
  v = "pid3final", items = item_list, label = "Partisanship",
  raw = FALSE, weight = "w"
)

df <- rbind(
  summ_avg_rank_by_group(
    main, 
    v = "pid3final", items = item_list$item, label = "Partisanship", 
    mean_only = FALSE
  ),
  summ_avg_rank_by_group(
    dt_w,
    v = "pid3final", items = item_list, label = "Partisanship",
    raw = FALSE, weight = "w", mean_only = FALSE
  )
) %>%
  mutate(item = factor(item, levels = rev(item_list$item)))

p <- viz_avg_rank_temp(df, wrap = "Partisanship")
pdf_default(p) + 
  theme(legend.position = "bottom")
ggsave(here("fig", "avg_rank_polar_by_pid.pdf"), width = 7, height = 2.5)

## By race ---------------------------------------------------------------------
summ_avg_rank_by_group(
  main,
  v = "race4labeled", items = item_list$item, label = "Race"
)
summ_avg_rank_by_group(
  dt_w,
  v = "race4labeled", items = item_list, label = "Race",
  raw = FALSE, weight = "w", mean_only = FALSE
)

summ_avg_rank_by_group(
  dt_w,
  v = "white", items = item_list, label = "Race",
  raw = FALSE, weight = "w", mean_only = FALSE
)

df <- rbind(
  summ_avg_rank_by_group(
    main, 
    v = "race4labeled", items = item_list$item, label = "Race", 
    mean_only = FALSE
  ),
  summ_avg_rank_by_group(
    dt_w,
    v = "race4labeled", items = item_list, label = "Race",
    raw = FALSE, weight = "w", mean_only = FALSE
  )
) %>%
  mutate(
    item = factor(item, levels = rev(item_list$item)),
    Race = factor(
      Race, levels = c("White", "Black", "Hispanic", "Other race"),
      labels = c("White", "Black", "Hispanic/Latino", "Other")
    )
  )

p <- viz_avg_rank_temp(df, wrap = "Race")
pdf_default(p) + 
  theme(legend.position = "bottom")
ggsave(here("fig", "avg_rank_polar_by_race.pdf"), width = 7, height = 3.5)

## By political interest -------------------------------------------------------
summ_avg_rank_by_group(
  main,
  v = "newsint_labeled", items = item_list$item,
  label = "Follow Politics (Political Interest)"
)

df <- rbind(
  summ_avg_rank_by_group(
    main, 
    v = "newsint_labeled", items = item_list$item, 
    label = "Follow Politics (Political Interest)", 
    mean_only = FALSE
  ),
  summ_avg_rank_by_group(
    dt_w,
    v = "newsint_labeled", items = item_list, 
    label = "Follow Politics (Political Interest)",
    raw = FALSE, weight = "w", mean_only = FALSE
  )
) %>%
  mutate(
    item = factor(item, levels = rev(item_list$item)),
    `Follow Politics (Political Interest)` = factor(
      `Follow Politics (Political Interest)`, 
      levels = c(
        "Most of the time", "Some of the time", 
        "Only now and then", "Hardly at all"
      )
    )
  )

p <- viz_avg_rank_temp(df, wrap = "`Follow Politics (Political Interest)`")
pdf_default(p) + 
  theme(legend.position = "bottom")
ggsave(here("fig", "avg_rank_polar_by_newsint.pdf"), width = 7, height = 3.5)

# PID: chi-squared test ========================================================
main_binary <- main %>%
  filter(!(pid3final == "Independent")) %>%
  mutate(pid3final = as.character(pid3final))

chisq.test(table(main_binary$pid3final, main_binary$app_polar_1))
chisq.test(table(main_binary$pid3final, main_binary$app_polar_2))
chisq.test(table(main_binary$pid3final, main_binary$app_polar_3)) ## p 0.05688
chisq.test(table(main_binary$pid3final, main_binary$app_polar_4))
chisq.test(table(main_binary$pid3final, main_binary$app_polar_5))

chisq.test(table(main$pid3final, main$app_polar_1))
chisq.test(table(main$pid3final, main$app_polar_2))
chisq.test(table(main$pid3final, main$app_polar_3)) ## p 0.1416
chisq.test(table(main$pid3final, main$app_polar_4)) ## p 0.07075
chisq.test(table(main$pid3final, main$app_polar_5))

# Race: chi-squared test =======================================================
temp <- dt_w %>%
  filter(race4labeled %in% c("White", "Black")) %>%
  mutate(race4labeled = as.character(race4labeled)) %>%
  mutate(race4numeric = ifelse(race4labeled == "White", 0, 1))

chisq.test(table(temp$race4labeled, temp$app_polar_1))
chisq.test(table(temp$race4labeled, temp$app_polar_2))
chisq.test(table(temp$race4labeled, temp$app_polar_3))
chisq.test(table(temp$race4labeled, temp$app_polar_4))
chisq.test(table(temp$race4labeled, temp$app_polar_5))

wtd.t.test(temp$politician_top, temp$race4numeric, weight = temp$w)
wtd.t.test(temp$media_top, temp$race4numeric, weight = temp$w)
wtd.t.test(temp$social_top, temp$race4numeric, weight = temp$w)
wtd.t.test(temp$interest_top, temp$race4numeric, weight = temp$w)
wtd.t.test(temp$citizen_top, temp$race4numeric, weight = temp$w)

chisq.test(table(main$race4labeled, main$app_polar_1))
chisq.test(table(main$race4labeled, main$app_polar_2))
chisq.test(table(main$race4labeled, main$app_polar_3))
chisq.test(table(main$race4labeled, main$app_polar_4))
chisq.test(table(main$race4labeled, main$app_polar_5))

t.test(politician_top ~ white, data = main)
t.test(media_top ~ white, data = main)
t.test(social_top ~ white, data = main)
t.test(interest_top ~ white, data = main)
t.test(citizen_top ~ white, data = main)

# Political interest: chi-squared test =========================================
chisq.test(table(main$newsint, main$app_polar_1))
chisq.test(table(main$newsint, main$app_polar_2)) ## p 0.2804
chisq.test(table(main$newsint, main$app_polar_3)) ## p 0.1898
chisq.test(table(main$newsint, main$app_polar_4)) ## p 0.5046
chisq.test(table(main$newsint, main$app_polar_5))

prop(main, "newsint_labeled")

temp <- dt_w %>%
  mutate(
    newsint_binary = case_when(
      newsint_labeled == "Hardly at all" ~ 1,
      TRUE ~ 0
    )
  )

wtd.t.test(temp$politician_top, temp$newsint_binary, weight = temp$w)
wtd.t.test(temp$media_top, temp$newsint_binary, weight = temp$w)
wtd.t.test(temp$social_top, temp$newsint_binary, weight = temp$w)
wtd.t.test(temp$interest_top, temp$newsint_binary, weight = temp$w) ## p 0.4350
wtd.t.test(temp$citizen_top, temp$newsint_binary, weight = temp$w)
