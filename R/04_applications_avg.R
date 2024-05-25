source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

# Setup ========================================================================
prep_list <- root_var %>%
  imap(
    ~ {
      full <- main %>%
        select(
          contains(paste0("app_", str_sub(.y, 1, 2))),
          matches(paste0("anc_", str_sub(.y, 1, 2))),
          matches(paste0("anc_correct_", str_sub(.y, 1, 2))),
          matches(paste0("anc_", str_sub(.y, 1, 2), ".*_recorded")),
          pid3final, partisan, race4asians, race2,
          berinsky_fail, ternovski_fail,
          berinsky_fail_label, ternovski_fail_label
        ) %>%
        select(-contains("row_rnd")) %>%
        select(-matches("^inputstate$"))
      crosswalk <- option_crosswalk[option_crosswalk %in% names(full)]
      full <- full %>% rename(!!!crosswalk)

      rational <- full %>%
        filter(
          !!as.name(paste0("app_", .y)) ==
            !!as.name(paste0("app_", .y, "_repeat"))
        )

      full <- full %>%
        select(-contains("repeat"))

      return(
        list(
          full = full, rational = rational,
          labels = names(crosswalk),
          full_N = nrow(full), rational_N = nrow(rational)
        )
      )
    }
  )
save(prep_list, file = here("data", "tidy", "prep_applications_list.Rda"))

# Average rankings =============================================================
## Quick summary
prep_list %>%
  imap(
    ~ avg_rank(.x$full, paste0("app_", .y))%>%
      select(item, mean) %>%
      ## transpose dataframe, with `name` column as column names
      pivot_wider(names_from = item, values_from = mean)
  )

# $tate
# 1st 2nd 3rd
# 1.7 2.0 2.2
#
# $identity
# 1st 2nd 3rd 4th
# 3.0 2.6 1.9 2.5
#
# $polar
# 1st 2nd 3rd 4th 5th
# 2.2 2.6 2.9 3.1 4.2
#
# $esystems
# 1st 2nd 3rd 4th 5th 6th
# 2.7 2.7 4.1 4.1 4.3 3.1

# Avg., pair, top-k, and marginal rankings =====================================
extrafont::loadfonts()
p1 <- viz_ranking(
  dat = prep_list$tate$full,
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork"),
  font = "CM Roman"
)

p2 <- viz_ranking(
  dat = prep_list$identity$full,
  target_item = "party",
  other_items = setdiff(prep_list$identity$labels, "party"),
  font = "CM Roman"
)

p3 <- viz_ranking(
  dat = prep_list$polar$full,
  target_item = "social_media",
  other_items = setdiff(prep_list$polar$labels, "social_media"),
  font = "CM Roman"
)

p4 <- viz_ranking(
  dat = prep_list$esystems$full,
  target_item = "account_pol",
  other_items = setdiff(prep_list$esystems$labels, "account_pol"),
  font = "CM Roman"
)

print(p1)
ggsave(
  here("fig", "tate_pork_emphasized.pdf"),
  width = 6.5, height = 4
)

print(p2)
ggsave(
  here("fig", "identity_party_emphasized.pdf"),
  width = 6.5, height = 4
)

print(p3)
ggsave(
  here("fig", "polar_social_media_emphasized.pdf"),
  width = 6.5, height = 4
)

print(p4)
ggsave(
  here("fig", "esystems_account_pol_emphasized.pdf"),
  width = 6.5, height = 4
)

# Bias corrections =============================================================
## Bootstrap
corrected_avg_list_asymp <- root_var %>%
  imap(
    ~ imprr(
      dat = prep_list[[.y]]$full,
      main_q = paste0("app_", .y),
      anchor_q = paste0("anc_", .y),
      anc_correct = paste0("anc_correct_", .y),
      main_labels = prep_list[[.y]]$labels,
      n_bootstrap = 1000
    )
  )
save(
  corrected_avg_list_asymp,
  file = here("output", "corrected_avg_list_asymp.Rda")
)

## Visualize average rankings based on raw and debiased data
corrected_avg_list_asymp %>%
  map("avg") %>%
  imap(
    ~ {
      print(viz_avg_wrapper(.x))
      ggsave_temp(paste0("corrected_avg_asymp_", .y, ".pdf"))
    }
  )

## What is the proportion of random vs. nonrandom answers?
corrected_avg_list_asymp %>% map("p_non_random") %>% bind_rows(.id = "app")
#        app       est       low        up
# 1     tate 0.6148466 0.6137987 0.6158944
# 2 identity 0.6846314 0.6837502 0.6855125
# 3    polar 0.7953994 0.7946017 0.7961972
# 4 esystems 0.5628818 0.5619383 0.5638253

# Save weights =================================================================
c(raw = "A. Raw Data", corrected = "B. Bias Corrected") %>%
  set_names(., .) %>%
  imap(
    ~ corrected_avg_list_asymp$tate$PMF %>%
      filter(imp == .x) %>%
      rename(
        !!as.name(paste0(est, "_", .y)) := est,
        !!as.name(paste0(low, "_", .y)) := low,
        !!as.name(paste0(up, "_", .y)) := up
      )
  ) ## %>%
  ## Reduce()
