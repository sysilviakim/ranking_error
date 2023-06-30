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

crosswalk_short <- function(x, label) {
  x %>%
    map(~.x) %>%
    bind_rows(.id = "reference") %>%
    rowwise() %>%
    mutate(varname = paste0("app_", label, "_", substr(reference, 1, 1))) %>%
    mutate(
      crosswalk = names(option_crosswalk)[which(option_crosswalk == varname)]
    ) %>%
    ungroup() %>%
    select(reference, varname, crosswalk, everything()) %>%
    ## Must refactor code
    mutate(
      name = crosswalk, 
      est = mean,
      low = mean - 1.96 * se,
      up = mean + 1.96 * se,
      imp = ""
    )
}

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
          pid3final, partisan, race4asians, race2, berinsky_fail, ternovski_fail
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
      crosswalk_short(., .y)
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

corrected_avg_list_asymp %>%
  map("est") %>%
  imap(
    ~ {
      print(viz_avg(.x, order = "fixed"))
      ggsave_temp(paste0("corrected_avg_asymp_", .y, ".pdf"))
    }
  )

corrected_avg_list_asymp %>% map("p_non_random") %>% bind_rows(.id = "app")
#        app       est       low        up
# 1     tate 0.5740763 0.5728997 0.5752529
# 2 identity 0.6531275 0.6521123 0.6541427
# 3    polar 0.7692743 0.7684214 0.7701272
# 4 esystems 0.5366934 0.5356606 0.5377261

corrected_avg_list_asymp$identity$est %>%
  group_split(imp, .keep = TRUE) %>%
  `names<-`({.} %>% map(~ .x$imp[1]) %>% unlist()) %>%
  imap(
    ~ {
      print(viz_avg(., order = "fixed", J = 4, color_list = "black"))
      ggsave_temp(
        paste0("avg_asymp_identity_", gsub(" ", "_", .y), ".pdf"),
        width = 4.5, height = 2
      )
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
  map("est") %>%
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
  map("est") %>%
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
  map("est") %>%
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
  map("est") %>%
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
      group_split(race4asians, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$race4asians[1]) %>% unlist()) %>%
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
  map("est") %>%
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
        viz_avg(
          corrected_avg_list_asymp_race[[.y]]$Asian,
          order = "fixed"
        ) +
          ggtitle("Asian"),
        ncol = 2, nrow = 2
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_race.pdf"),
        width = 11, height = 6
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
  map("est") %>%
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

# Alternative anchors ==========================================================
id_alphabet <- imprr(
  dat = prep_list$identity$full %>% filter(!is.na(anc_id_alphabet)),
  main_q = "app_identity",
  anchor_q = "anc_id_alphabet",
  anc_correct = "anc_correct_id_alphabet",
  main_labels = prep_list$identity$labels,
  asymptotics = TRUE
)
viz_avg(id_alphabet, order = "fixed")

id_exact <- imprr(
  dat = prep_list$identity$full %>% filter(!is.na(anc_id_exact)),
  main_q = "app_identity",
  anchor_q = "anc_id_exact",
  anc_correct = "anc_correct_id_exact",
  main_labels = prep_list$identity$labels,
  asymptotics = TRUE
)
viz_avg(id_exact, order = "fixed") ## religion

es_alphabet <- imprr(
  dat = prep_list$esystems$full %>% filter(!is.na(anc_es_alphabet)),
  main_q = "app_esystems",
  anchor_q = "anc_es_alphabet",
  anc_correct = "anc_correct_es_alphabet",
  main_labels = prep_list$esystems$labels,
  asymptotics = TRUE
)
viz_avg(es_alphabet, order = "fixed")

es_temporal <- imprr(
  dat = prep_list$esystems$full %>% filter(!is.na(anc_es_temporal)),
  main_q = "app_esystems",
  anchor_q = "anc_es_temporal",
  anc_correct = "anc_correct_es_temporal",
  main_labels = prep_list$esystems$labels,
  asymptotics = TRUE
)
viz_avg(es_temporal, order = "fixed")

# By attention checks ==========================================================
corrected_avg_list_asymp_berinsky_fail <- root_var %>%
  imap(
    ~ prep_list[[.y]]$full %>%
      mutate(
        berinsky_fail = case_when(
          berinsky_fail == 1 ~ "Failed",
          TRUE ~ "Passed"
        )
      ) %>%
      group_split(berinsky_fail, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$berinsky_fail[1]) %>% unlist()) %>%
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
  corrected_avg_list_asymp_berinsky_fail,
  file = here("output", "corrected_avg_list_asymp_berinsky_fail.Rda")
)

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        viz_avg(
          corrected_avg_list_asymp_berinsky_fail[[.y]]$Failed,
          order = "fixed"
        ) +
          ggtitle("Failed"),
        viz_avg(
          corrected_avg_list_asymp_berinsky_fail[[.y]]$Passed,
          order = "fixed"
        ) +
          ggtitle("Passed"),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_berinsky.pdf"),
        height = 6
      )
    }
  )

corrected_avg_list_asymp_ternovski_fail <- root_var %>%
  imap(
    ~ prep_list[[.y]]$full %>%
      mutate(
        ternovski_fail = case_when(
          ternovski_fail == 1 ~ "Failed",
          TRUE ~ "Passed"
        )
      ) %>%
      group_split(ternovski_fail, .keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x$ternovski_fail[1]) %>% unlist()) %>%
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
  corrected_avg_list_asymp_ternovski_fail,
  file = here("output", "corrected_avg_list_asymp_ternovski_fail.Rda")
)

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        viz_avg(
          corrected_avg_list_asymp_ternovski_fail[[.y]]$Failed,
          order = "fixed"
        ) +
          ggtitle("Failed"),
        viz_avg(
          corrected_avg_list_asymp_ternovski_fail[[.y]]$Passed,
          order = "fixed"
        ) +
          ggtitle("Passed"),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("corrected_avg_asymp_", .y, "_ternovski.pdf"),
        height = 6
      )
    }
  )

# By anchor passes =============================================================
## Uncorrected versions; raw
avg_list_anchor_fail <- root_var %>%
  imap(
    ~ {
      out <- prep_list[[.y]]$full %>%
        mutate(
          !!as.name(paste0("anc_correct_", .y)) := case_when(
            !!as.name(paste0("anc_correct_", .y)) == 1 ~ "Passed",
            TRUE ~ "Failed"
          )
        ) %>%
        group_split(!!as.name(paste0("anc_correct_", .y)), .keep = TRUE) %>%
        `names<-`(
          {.} %>% 
            map(
              function(x, y) x[[paste0("anc_correct_", .y)]][[1]]
            ) %>% 
            unlist()
        )
      
      list(
        Passed = avg_rank(out$Passed, paste0("app_", .y)) %>%
          crosswalk_short(., .y),
        Failed = avg_rank(out$Failed, paste0("app_", .y)) %>%
          crosswalk_short(., .y)
      )
    }
  )

root_var %>%
  imap(
    ~ {
      p <- ggarrange(
        plot_nolegend(
          viz_avg(
            avg_list_anchor_fail[[.y]]$Passed,
            order = "fixed", J = nrow(avg_list_anchor_fail[[.y]]$Passed)
          ) +
            ggtitle("Passed")
        ),
        plot_nolegend(
          viz_avg(
            avg_list_anchor_fail[[.y]]$Failed,
            order = "fixed", J = nrow(avg_list_anchor_fail[[.y]]$Failed)
          ) +
            ggtitle("Failed")
        ),
        ncol = 1
      )
      pdf_short(p)
      ggsave_temp(
        paste0("avg_list_", .y, "_anchor_fail.pdf"),
        height = 6
      )
    }
  )

