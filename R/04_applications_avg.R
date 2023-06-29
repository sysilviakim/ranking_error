source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
main <- df_list$main

ggsave_temp <- function(x, width = 5.5, height = 3) {
  ggsave(here("fig", x), width = width, height = height)
}

pdf_short <- function(p) {
  pdf_default(p) +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
}

# Setup ========================================================================
prep_list <- root_var %>%
  imap(
    ~ {
      full <- main %>%
        select(
          contains(paste0("app_", .y)),
          matches(paste0("anc_", .y)),
          matches(paste0("anc_correct_", .y, "$")),
          matches(paste0("anc_", .y, "_recorded")),
          pid3final,
          partisan,
          race4,
          race2
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
  other_items = setdiff(prep_list$tate$labels, "pork"),
  order = "est"
)

p2 <- viz_ranking(
  dat = prep_list$identity$full,
  target_item = "party",
  other_items = setdiff(prep_list$identity$labels, "party"),
  order = "est"
)

p3 <- viz_ranking(
  dat = prep_list$polar$full,
  target_item = "social_media",
  other_items = setdiff(prep_list$polar$labels, "social_media"),
  order = "est"
)

p4 <- viz_ranking(
  dat = prep_list$esystems$full,
  target_item = "account_pol",
  other_items = setdiff(prep_list$esystems$labels, "account_pol"),
  order = "est"
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

corrected_avg_list_asymp %>%
  imap(
    ~ {
      print(viz_avg(.x, order = "est"))
      ggsave_temp(paste0("corrected_avg_asymp_", .y, ".pdf"))
    }
  )

# Consistent-preference respondents subset =====================================
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

corrected_avg_list_asymp_rational %>%
  imap(
    ~ {
      print(viz_avg(.x, order = "est"))
      ggsave_temp(paste0("corrected_avg_asymp_", .y, "_rational.pdf"))
    }
  )

# By party =====================================================================
corrected_avg_list_asymp_pid3 <- root_var %>%
  imap(
    ~ prep_list[[.y]]$full %>%
      group_split(pid3final, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$pid3final[1]) %>% unlist()) %>%
      imap(
        function(x, y) {
          imprr(
            dat = x,
            main_q = paste0("app_", .y),
            anchor_q = paste0("anc_", .y),
            anc_correct = paste0("anc_correct_", .y),
            main_labels = prep_list[[.y]]$labels,
            asymptotics = TRUE
          )
        }
      )
  )
save(
  corrected_avg_list_asymp_pid3,
  file = here("output", "corrected_avg_list_asymp_pid3.Rda")
)

corrected_avg_list_asymp_pid3 %>%
  imap(
    ~ .x %>%
      imap(
        function(x, y) {
          print(viz_avg(x, order = "est"))
          ggsave_temp(
            paste0(
              "corrected_avg_asymp_", .y, "_pid3_", 
              gsub(" ", "_", tolower(y)), ".pdf"
            )
          )
        }
      )
  )

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        viz_avg(
          corrected_avg_list_asymp_pid3[[.y]]$Democrat,
          order = "fixed"
        ) +
          ggtitle("Democrat"),
        viz_avg(
          corrected_avg_list_asymp_pid3[[.y]]$Republican,
          order = "fixed"
        ) +
          ggtitle("Republican"),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_pid3.pdf"),
        height = 6
      )
    }
  )

# By strength of partisanship ==================================================
corrected_avg_list_asymp_partisan <- root_var %>%
  imap(
    ~ prep_list[[.y]]$full %>%
      group_split(partisan, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$partisan[1]) %>% unlist()) %>%
      imap(
        function(x, y) {
          imprr(
            dat = x,
            main_q = paste0("app_", .y),
            anchor_q = paste0("anc_", .y),
            anc_correct = paste0("anc_correct_", .y),
            main_labels = prep_list[[.y]]$labels,
            asymptotics = TRUE
          )
        }
      )
  )
save(
  corrected_avg_list_asymp_partisan,
  file = here("output", "corrected_avg_list_asymp_partisan.Rda")
)

corrected_avg_list_asymp_partisan %>%
  imap(
    ~ .x %>%
      imap(
        function(x, y) {
          print(viz_avg(x))
          ggsave_temp(
            paste0(
              "corrected_avg_asymp_", .y, "_partisan_", 
              gsub(" ", "_", tolower(y)), ".pdf"
            )
          )
        }
      )
  )

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        viz_avg(
          corrected_avg_list_asymp_partisan[[.y]]$`Strong Partisan`,
          order = "fixed"
        ) +
          ggtitle("Strong Partisan"),
        viz_avg(
          corrected_avg_list_asymp_partisan[[.y]]$`Weak Partisan`,
          order = "fixed"
        ) +
          ggtitle("Weak Partisan"),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_partisan.pdf"),
        height = 6
      )
    }
  )

# By race ======================================================================
corrected_avg_list_asymp_race <- root_var %>%
  imap(
    ~ prep_list[[.y]]$full %>%
      group_split(race4, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$race4[1]) %>% unlist()) %>%
      imap(
        function(x, y) {
          imprr(
            dat = x,
            main_q = paste0("app_", .y),
            anchor_q = paste0("anc_", .y),
            anc_correct = paste0("anc_correct_", .y),
            main_labels = prep_list[[.y]]$labels,
            asymptotics = TRUE
          )
        }
      )
  )
save(
  corrected_avg_list_asymp_race,
  file = here("output", "corrected_avg_list_asymp_race.Rda")
)

corrected_avg_list_asymp_race %>%
  imap(
    ~ .x %>%
      imap(
        function(x, y) {
          print(viz_avg(x))
          ggsave_temp(
            paste0(
              "corrected_avg_asymp_", .y, "_race_", 
              gsub(" ", "_", tolower(y)), ".pdf"
            )
          )
        }
      )
  )

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        viz_avg(
          corrected_avg_list_asymp_race[[.y]]$White,
          order = "fixed"
        ) +
          ggtitle("White"),
        viz_avg(
          corrected_avg_list_asymp_race[[.y]]$Black,
          order = "fixed"
        ) +
          ggtitle("Black"),
        viz_avg(
          corrected_avg_list_asymp_race[[.y]]$Hispanic,
          order = "fixed"
        ) +
          ggtitle("Hispanic"),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_race.pdf"),
        height = 9
      )
    }
  )

# By binary race ===============================================================
corrected_avg_list_asymp_race2 <- root_var %>%
  imap(
    ~ prep_list[[.y]]$full %>%
      group_split(race2, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$race2[1]) %>% unlist()) %>%
      imap(
        function(x, y) {
          imprr(
            dat = x,
            main_q = paste0("app_", .y),
            anchor_q = paste0("anc_", .y),
            anc_correct = paste0("anc_correct_", .y),
            main_labels = prep_list[[.y]]$labels,
            asymptotics = TRUE
          )
        }
      )
  )
save(
  corrected_avg_list_asymp_race2,
  file = here("output", "corrected_avg_list_asymp_race2.Rda")
)

corrected_avg_list_asymp_race2 %>%
  imap(
    ~ .x %>%
      imap(
        function(x, y) {
          print(viz_avg(x))
          ggsave_temp(
            paste0(
              "corrected_avg_asymp_", .y, "_race2_", 
              gsub(" ", "_", tolower(y)), ".pdf"
            )
          )
        }
      )
  )

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        viz_avg(
          corrected_avg_list_asymp_race2[[.y]]$White,
          order = "fixed"
        ) +
          ggtitle("White"),
        viz_avg(
          corrected_avg_list_asymp_race2[[.y]]$`None-white`,
          order = "fixed"
        ) +
          ggtitle("None-white"),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_race2.pdf"),
        height = 6
      )
    }
  )
