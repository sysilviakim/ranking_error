source(here::here("R", "utilities.R"))
# df_list <- qualtrics_import("pretest-01-sanitized-numeric.csv")
df_list <- qualtrics_import("pretest-02.csv")
main <- df_list$main

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
assert_that(nchar(prep_list$tate1993$answer) == 3)

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
## For voting and identity, not much point in visualizing
print(uniform_list$tate1993$main)
ggsave(here("fig", "pretest-tate1993-main.pdf"), width = 4.5, height = 2.8)

print(uniform_list$tate1993$anch)
ggsave(here("fig", "pretest-tate1993-anchor.pdf"), width = 4.5, height = 2.8)

print(uniform_list$nelson1997$main)
ggsave(here("fig", "pretest-nelson1997-main.pdf"), width = 7, height = 2.8)

print(uniform_list$nelson1997$anch)
ggsave(here("fig", "pretest-nelson1997-anchor.pdf"), width = 7, height = 2.8)

# Decontaminating: \pi =========================================================

#### Yuki comment: These two graphs offer a great test for Assumptions 2-3
#### It seems to me that one of them (or both) is violated.
#### If non-sincere responses have the same distribution, we should able to
#### observe similar empirical distributions in the two figures
#### One solution is to randomize the order between the anchor and main Qs
#### (Let's try it in our pre-test)

## Create pi dataset -----------------------------------------------------------
pi_list <- prep_list %>%
  imap(
    ~ {
      N <- nrow(.x$dat)
      J <- nchar(.x$dat[[paste0("app_", .y)]][[1]])

      ## Naive estimator 1: heroic assumption of zero non-sincere responses
      est_main_naive_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            table(
              .x$dat[[paste0("app_", .y)]][unlist(.x$indices[, x])]
            ) / N
          }
        )

      ## Unbiased estimation of the proportion of sincere responses (Eq. 9)
      est_p_z1_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            (mean(.x$correct[unlist(.x$indices[, x])]) - (1 / factorial(J))) /
              (1 - (1 / factorial(J)))
          }
        )

      ## Check that this is a valid probability
      assert_that(all(est_p_z1_boot >= 0))
      assert_that(all(est_p_z1_boot <= 1))

      ## Unbiased estimator for the underlying non-sincere dist. in anchor q.
      ## (Eq. 10)
      est_anch_naive_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            (
              permn_augment(
                table(.x$dat[[paste0("anc_", .y)]][unlist(.x$indices[, x])]),
                J = J
              ) / N -
                (est_p_z1_boot[[x]]) * c(1, rep(0, factorial(J) - 1))
            ) / (1 - est_p_z1_boot[[x]])
          }
        )
      # est_anch_naive_boot <- seq(bootstrap_n) %>%
      #   map(~ rep(1 / factorial(J), factorial(J)))

      assert_that(all(near(est_anch_naive_boot %>% map(sum) %>% unlist(), 1)))
      assert_that(all(est_anch_naive_boot %>% map(min) %>% unlist() >= 0))
      assert_that(all(est_anch_naive_boot %>% map(max) %>% unlist() <= 1))

      est_pi_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            (
              permn_augment(est_main_naive_boot[[x]], J = J) -
                (1 - est_p_z1_boot[[x]]) * est_anch_naive_boot[[x]]
            ) /
              est_p_z1_boot[[x]]
          }
        )
      ## These might not be met...
      # assert_that(all(est_pi_boot %>% map(sum) %>% unlist() == 1))
      assert_that(all(near(est_pi_boot %>% map(sum) %>% unlist(), 1)))
      assert_that(all(est_pi_boot %>% map(min) %>% unlist() >= 0))
      assert_that(all(est_pi_boot %>% map(max) %>% unlist() <= 1))

      ## Recovering reference rankings from respondents who got the right answer
      correct_answer_boot <- seq(bootstrap_n) %>%
        ## No recov_ref_ranking necessary,
        ## as Qualtrics has done that step already
        map(
          function(x) {
            .x$dat[[paste0("app_", .y)]][unlist(.x$indices[, x])][
              .x$correct[unlist(.x$indices[, x])] == 1
            ]
          }
        )

      ## Naive estimator 2: simply use Rs with correct responses to anchor q
      est_correct_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            table(correct_answer_boot[[x]]) /
              length(correct_answer_boot[[x]])
          }
        )

      ggdat_boot <- seq(bootstrap_n) %>%
        map(
          ~ rbind(
            est_main_naive_boot[[.x]], est_correct_boot[[.x]], est_pi_boot[[.x]]
          ) %>%
            as_tibble() %>%
            mutate(
              est = c("Naive Estimator 1", "Naive Estimator 2", "Our Method")
            ) %>%
            pivot_longer(
              cols = !est,
              names_to = "ranking",
              values_to = "proportion"
            )
        )

      summ_boot <- seq(nrow(ggdat_boot[[1]])) %>%
        map(
          ~ {
            vec <- ggdat_boot %>%
              map(function(x) x$proportion[.x]) %>%
              unlist()
            return(
              tibble(
                avg = mean(vec),
                sd = sd(vec),
                se = sd / sqrt(bootstrap_n),
                low = quantile(vec, p = 0.025),
                upp = quantile(vec, p = 0.975)
              )
            )
          }
        ) %>%
        bind_rows() %>%
        bind_cols(ggdat_boot[[1]] %>% select(-proportion), .) %>%
        mutate(se = ifelse(est == "Quantity of Interest", NA, se))

      return(summ_boot)
    }
  )

## Visualization ---------------------------------------------------------------
p_list <- pi_list %>%
  map(
    ~ {
      p <- ggplot(.x, aes(x = ranking, y = avg, fill = est)) +
        geom_bar(stat = "identity", position = "dodge2", alpha = 0.7) +
        geom_errorbar(
          position = position_dodge(width = 0.9), width = 0.25,
          aes(ymin = low, ymax = upp)
        ) +
        scale_fill_manual(values = c("gray70", "gray10", "firebrick4")) +
        xlab("") +
        ylab("") +
        scale_y_continuous(
          limits = c(-0.4, 1), labels = scales::percent,
          breaks = seq(-0.4, 1, by = 0.2)
        ) +
        theme_classic() +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          plot.margin = margin(0.2, 0.2, 0, -0.2, "cm")
        )
      return(plot_notitle(pdf_default(p)))
    }
  )

print(p_list$tate1993)
ggsave(
  here("fig", "application-tate1993-bootstrapped-pi.pdf"),
  width = 5.5, height = 4
)

# Too many to be meaningful: factorial(7)
# print(p_list$identity)
# ggsave(
#   here("fig", "application-identity-bootstrapped-pi.pdf"),
#   width = 5.5, height = 4
# )

print(
  p_list$nelson1997 +
    scale_y_continuous(
      limits = c(-0.2, 0.415), labels = scales::percent,
      breaks = seq(-0.2, 0.415, by = 0.2)
    )
)
ggsave(
  here("fig", "application-nelson1997-bootstrapped-pi.pdf"),
  width = 10, height = 4
)

# Too many to be meaningful: factorial(5)
# print(p_list$voting)
# ggsave(
#   here("fig", "application-voting-bootstrapped-pi.pdf"),
#   width = 5.5, height = 4
# )

# Decontaminating: avg. rank ===================================================
## Create avg. rank dataset ----------------------------------------------------
avg_rank_list <- prep_list %>%
  imap(
    ~ {
      ## Naive estimator 1: heroic assumption of zero non-sincere responses
      est_main_naive_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            avg_rank(
              .x$dat[unlist(.x$indices[, x]), ], paste0("app_", .y)
            )
          }
        ) %>%
        bind_rows()

      J <- nchar(.x$answer)

      ## Unbiased estimation of the proportion of sincere responses (Eq. 9)
      est_p_z1_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            (mean(.x$correct[unlist(.x$indices[, x])]) - (1 / factorial(J))) /
              (1 - (1 / factorial(J)))
          }
        )

      ## Check that this is a valid probability
      assert_that(all(est_p_z1_boot >= 0))
      assert_that(all(est_p_z1_boot <= 1))

      ## Turn into a vector
      vec_pr_z1 <- unlist(est_p_z1_boot)
      vec_pr_z0 <- 1 - vec_pr_z1

      ## Unbiased estimator for the underlying non-sincere dist. in anchor q.
      ## (Eq. 10)
      est_anch_naive_boot <- seq(bootstrap_n) %>%
        map(
          function(x) {
            avg_rank(
              .x$dat[unlist(.x$indices[, x]), ], paste0("anc_", .y)
            )
          }
        ) %>%
        bind_rows()

      ## De-contaminated ranking data
      est_debiased_boot <-
        (est_main_naive_boot - vec_pr_z0 * est_anch_naive_boot) / vec_pr_z1

      ## Quantiles calculated and into a single dataframe
      boot_list <- list(
        debiased = est_debiased_boot, naive = est_main_naive_boot
      )

      ## Application-specific labels
      if (.y == "tate1993") {
        boot_list <- boot_list %>%
          map(
            ~ .x %>%
              rename(
                Policy = `1st`, Service = `2nd`, Pork = `3rd`
              )
          )
      } else if (.y == "identity") {
        boot_list <- boot_list %>%
          map(
            ~ .x %>%
              rename(
                City = `1st`, SES = `2nd`, Gender = `3rd`,
                Party = `4th`, Race = `5th`, Religion = `6th`, US = `7th`
              )
          )
      } else if (.y == "nelson1997") {
        boot_list <- boot_list %>%
          map(
            ~ .x %>%
              rename(
                `Freedom of Speech` = `1st`,
                Security = `2nd`,
                Reputation = `3rd`,
                `Anti-racism` = `4th`
              )
          )
      } else if (.y == "voting") {
        boot_list <- boot_list %>%
          map(
            ~ .x %>%
              rename(
                `Election Day` = `1st`,
                Early = `2nd`,
                VBM = `3rd`,
                `VBM Dropoff` = `4th`,
                `Not Vote` = `5th`
              )
          )
      }

      ## Bootstrap
      summ <- avg_rank_bootstrap_quantile(boot_list)

      if (.y == "nelson1997") {
        summ <- summ %>%
          mutate(
            variable = case_when(
              variable == "Anti-racism" ~ "Anti-\nracism",
              variable == "Freedom of Speech" ~ "Freedom\nof Speech",
              TRUE ~ variable
            )
          )
      }

      ## Application-specific labels
      if (.y == "tate1993") {
        summ <- summ %>%
          mutate(
            variable = factor(
              variable,
              levels = c("Policy", "Service", "Pork")
            )
          )
      } else if (.y == "identity") {
        summ <- summ %>%
          ## Alphabetical, so this is good enough
          mutate(variable = factor(variable))
      } else if (.y == "nelson1997") {
        summ <- summ %>%
          mutate(
            variable = factor(
              variable,
              levels = c(
                "Freedom\nof Speech", "Security", "Reputation", "Anti-\nracism"
              )
            )
          )
      } else if (.y == "voting") {
        summ <- summ %>%
          mutate(
            variable = factor(
              variable,
              levels = c(
                "Election Day", "Early", "VBM", "VBM Dropoff", "Not Vote"
              )
            )
          )
      }

      summ <- summ %>%
        mutate(
          variable = factor(
            variable,
            labels = str_pad(levels(variable), width = 12)
          )
        )

      return(list(boot_list = boot_list, summ = summ, p = vec_pr_z1))
    }
  )

## Visualization ---------------------------------------------------------------
p_list <- avg_rank_list %>%
  map("summ") %>%
  map(
    ~ ggplot(.x, aes(x = fct_rev(variable), y = mean_val, color = Estimator)) +
      geom_point(
        aes(shape = Estimator),
        size = 3,
        position = position_dodge(width = 0.6)
      ) +
      # Reorder by point estimate
      geom_linerange(
        aes(ymin = low, ymax = up),
        lwd = 1.5, position = position_dodge(width = 0.6)
      ) +
      scale_color_manual(values = c("violetred3", "#999999")) +
      theme_bw() +
      xlab("") +
      ylab("")
  )

pdf_default(p_list$tate1993) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = .5)) +
  geom_hline(yintercept = 2, lty = "dashed", col = "gray") +
  theme(legend.position = "top") +
  coord_flip()
ggsave(
  here("fig/application-tate1993-bootstrapped-avg-rank.pdf"),
  width = 3.5, height = 2.8
)

pdf_default(p_list$nelson1997) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = .5)) +
  geom_hline(yintercept = 2.5, lty = "dashed", col = "gray") +
  theme(legend.position = "top") +
  coord_flip()
ggsave(
  here("fig/application-nelson1997-bootstrapped-avg-rank.pdf"),
  width = 3.5, height = 2.8
)

pdf_default(p_list$identity) +
  scale_y_continuous(
    limits = c(0, 7), breaks = seq(0, 7, by = 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  geom_hline(yintercept = 3.5, lty = "dashed", col = "gray") +
  theme(legend.position = "top") +
  coord_flip() +
  theme(axis.text = element_text(size = 12))
ggsave(
  here("fig/application-identity-bootstrapped-avg-rank.pdf"),
  width = 5, height = 2.8 * 1.5
)

pdf_default(p_list$voting) +
  scale_y_continuous(
    limits = c(-0.5, 6.5), breaks = seq(-0, 6, by = 1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  geom_hline(yintercept = 3, lty = "dashed", col = "gray") +
  theme(legend.position = "top") +
  coord_flip() +
  theme(axis.text = element_text(size = 12))
ggsave(
  here("fig/application-voting-bootstrapped-avg-rank.pdf"),
  width = 5, height = 2.8 * 1.5
)

## non-overlapping CI? ---------------------------------------------------------
## Although, interpret with caution: overlap doesn't necessarily mean yada yada
avg_rank_list$tate1993$summ %>% filter(variable == "Service")
avg_rank_list$nelson1997$summ %>% filter(variable == "Anti-\nracism")
avg_rank_list$identity$summ %>% filter(variable == "SES")
avg_rank_list$identity$summ %>% filter(variable == "US")
avg_rank_list$identity$summ %>% filter(variable == "City")
avg_rank_list$voting$summ %>% filter(variable == "VBM Dropoff")

# Corrected Pr(z = 1) ==========================================================
## Uncorrected
p1 <- prep_list %>%
  map("correct") %>%
  map(mean) %>%
  unlist() %>%
  formatC(., format = "f", digits = 4)
p1
# tate1993   identity nelson1997     voting
# "0.4900"   "0.4700"   "0.5100"   "0.3300"

## Corrected
p2 <- avg_rank_list %>%
  map(~ mean(.x$p)) %>%
  unlist() %>%
  formatC(., format = "f", digits = 4)
p2
# tate1993   identity nelson1997     voting
# "0.4866"   "0.4685"   "0.5072"   "0.3243"

## Export it to table
tab <- bind_rows(
  enframe(p1) %>% mutate(type = "Uncorrected"),
  enframe(p2) %>% mutate(type = "Corrected")
) %>%
  pivot_wider(id_cols = "type") %>%
  rename(
    Type = type,
    `Tate 1993` = tate1993,
    Identity = identity,
    `Nelson 1997` = nelson1997,
    `Voting Mode` = voting
  )

print.xtable(
  xtable(tab),
  include.rownames = FALSE,
  comment = FALSE,
  type = "latex",
  file = here("tab", "pr_z1_application.tex"),
  floating = FALSE,
  booktabs = TRUE
)
