source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

# Setup ========================================================================
prep_list <- root_var %>%
  imap(
    ~ {
      full <- main %>%
        select(
          contains(paste0("app_", .y)),
          matches(paste0("anc_", .y)),
          matches(paste0("anc_correct_", .y, "$")),
          matches(paste0("anc_", .y, "_recorded"))
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

# Average rankings =============================================================
## Quick summary
prep_list %>%
  imap(
    ~ avg_rank(.x$full, paste0("app_", .y)) %>%
      map_dbl("mean") %>%
      round(., digits = 1)
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
# 2.3 2.6 2.9 3.1 4.2
#
# $esystems
# 1st 2nd 3rd 4th 5th 6th
# 2.7 2.7 4.1 4.1 4.3 3.1

prep_list %>%
  imap(
    ~ avg_rank(.x$full, paste0("app_", .y)) %>%
      map(~.x) %>%
      bind_rows(.id = "reference") %>%
      rowwise() %>%
      mutate(varname = paste0("app_", .y, "_", substr(reference, 1, 1))) %>%
      mutate(
        crosswalk = names(option_crosswalk)[which(option_crosswalk == varname)]
      ) %>%
      ungroup() %>%
      select(reference, varname, crosswalk, everything())
  )

# Avg., pair, top-k, and marginal rankings =====================================
p1 <- viz_ranking(
  dat = prep_list$tate$full,
  target_item = "pork",
  other_items = setdiff(prep_list$tate$labels, "pork")
)

p2 <- viz_ranking(
  dat = prep_list$identity$full,
  target_item = "party",
  other_items = setdiff(prep_list$identity$labels, "party")
)

p3 <- viz_ranking(
  dat = prep_list$polar$full,
  target_item = "social_media",
  other_items = setdiff(prep_list$polar$labels, "social_media")
)

p4 <- viz_ranking(
  dat = prep_list$esystems$full,
  target_item = "account_pol",
  other_items = setdiff(prep_list$esystems$labels, "account_pol")
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
corrected_avg_list_asymp <- root_var %>%
  imap(
    ~ imprr(
      dat = prep_list[[.y]]$full,
      main_q = paste0("app_", .y),
      anchor_q = paste0("anc_", .y),
      anc_correct = paste0("anc_correct_", .y),
      main_labels = prep_list[[.y]]$labels,
      asymptotics = TRUE
    )
  )
save(
  corrected_avg_list_asymp,
  file = here("output", "corrected_avg_list_asymp.Rda")
)

corrected_avg_list_data <- root_var %>%
  imap(
    ~ imprr(
      dat = prep_list[[.y]]$full,
      main_q = paste0("app_", .y),
      anchor_q = paste0("anc_", .y),
      anc_correct = paste0("anc_correct_", .y),
      main_labels = prep_list[[.y]]$labels,
      asymptotics = FALSE
    )
  )
save(
  corrected_avg_list_data,
  file = here("output", "corrected_avg_list_data.Rda")
)

print(viz_avg(corrected_avg_list_asymp$tate))
ggsave(here("fig", "corrected_avg_asymp_tate.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_asymp$identity))
ggsave(here("fig", "corrected_avg_asymp_identity.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_asymp$polar))
ggsave(here("fig", "corrected_avg_asymp_polar.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_asymp$esystems))
ggsave(here("fig", "corrected_avg_asymp_esystems.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data$tate))
ggsave(here("fig", "corrected_avg_data_tate.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data$identity))
ggsave(here("fig", "corrected_avg_data_identity.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data$polar))
ggsave(here("fig", "corrected_avg_data_polar.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data$esystems))
ggsave(here("fig", "corrected_avg_data_esystems.pdf"), width = 5.5, height = 3)

# Bias corrections but consistent-preference respondents =======================
corrected_avg_list_asymp_rational <- root_var %>%
  imap(
    ~ imprr(
      dat = prep_list[[.y]]$rational,
      main_q = paste0("app_", .y),
      anchor_q = paste0("anc_", .y),
      anc_correct = paste0("anc_correct_", .y),
      main_labels = prep_list[[.y]]$labels,
      asymptotics = TRUE
    )
  )
save(
  corrected_avg_list_asymp_rational,
  file = here("output", "corrected_avg_list_asymp_rational.Rda")
)

corrected_avg_list_data_rational <- root_var %>%
  imap(
    ~ imprr(
      dat = prep_list[[.y]]$rational,
      main_q = paste0("app_", .y),
      anchor_q = paste0("anc_", .y),
      anc_correct = paste0("anc_correct_", .y),
      main_labels = prep_list[[.y]]$labels,
      asymptotics = FALSE
    )
  )
save(
  corrected_avg_list_data_rational,
  file = here("output", "corrected_avg_list_data_rational.Rda")
)

print(viz_avg(corrected_avg_list_asymp_rational$tate))
ggsave(here("fig", "corrected_avg_asymp_tate_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_asymp$identity))
ggsave(here("fig", "corrected_avg_asymp_identity_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_asymp_rational$polar))
ggsave(here("fig", "corrected_avg_asymp_polar_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_asymp_rational$esystems))
ggsave(here("fig", "corrected_avg_asymp_esystems_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data_rational$tate))
ggsave(here("fig", "corrected_avg_data_tate_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data_rational$identity))
ggsave(here("fig", "corrected_avg_data_identity_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data_rational$polar))
ggsave(here("fig", "corrected_avg_data_polar_rational.pdf"), width = 5.5, height = 3)

print(viz_avg(corrected_avg_list_data_rational$esystems))
ggsave(here("fig", "corrected_avg_data_esystems_rational.pdf"), width = 5.5, height = 3)
