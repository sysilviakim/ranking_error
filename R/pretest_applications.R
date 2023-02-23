source(here::here("R", "pretest_zero_weak_context.R"))

# Tate (1993) ==================================================================
## Uniform distribution test ===================================================
### Use order of randomized items to recover observed ranking ------------------
### First, turn text into item numbers from the reference choice set.
main <- main %>%
  mutate(
    across(
      contains("tate1993"),
      ~ gsub(
        paste0(
          "Federal government that create policies that affect people's lives at the federal level|",
          "Working in Congress on bills concerning national issues"
        ),
        "1",
        gsub(
          paste0(
            "State government that create policies that affect people's lives at the state level|",
            "Helping people in the district who have personal problems with government"
          ),
          "2",
          gsub(
            paste0(
              "Municipal government that create policies that affect people's lives at the city level|",
              "Making sure the state/district gets its fair share of government money and projects"
            ),
            "3",
            gsub("\\|", "", .x)
          )
        )
      )
    )
  )

### Collapse "resulting" ranking -----------------------------------------------
tate1993 <- main %>%
  mutate(
    across(
      anc_tate1993_1:app_tate1993_3,
      ~ case_when(.x == "-99" ~ "9", TRUE ~ .x)
    )
  ) %>%
  unite("anc_tate1993", sep = "", anc_tate1993_1:anc_tate1993_3) %>%
  unite("app_tate1993", sep = "", app_tate1993_1:app_tate1993_3) %>%
  ## Lost 10 obs (10.2%)
  filter(!grepl("9", anc_tate1993) & !grepl("9", app_tate1993))

### Recover the "observed" ranking ---------------------------------------------
## based on randomized order presentation
tate1993$anc_tate1993_obs <- tate1993 %>%
  separate(anc_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  recov_ref_ranking(rank_var = "anc_tate1993") %>%
  .$ref

tate1993$app_tate1993_obs <- tate1993 %>%
  separate(app_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  recov_ref_ranking(rank_var = "app_tate1993") %>%
  .$ref

### Create frequency tables ----------------------------------------------------
tab_main <- table(tate1993$app_tate1993_obs)
tab_anch <- table(tate1993$anc_tate1993_obs)

## Note that we've cleaned main and anchor questions' partial rankings
## simultaneously now, not separately
round(prop.table(tab_main) * 100, digits = 1)
#  123  132  213  231  312  321
# 19.3 11.4 25.0 12.5 14.8 17.0
round(prop.table(tab_anch) * 100, digits = 1)
#  123  132  213  231  312  321
# 20.5 14.8  8.0 15.9 20.5 20.5

### Chi-square test and power test ---------------------------------------------
chisq_power(tab_main) ## p-value = 0.2491, ES = 0.275, N = 260+
chisq_power(tab_anch) ## p-value = 0.2606, ES = 0.272, N = 270+

### Visualize distribution -----------------------------------------------------
temp <- table_to_tibble(tab_main)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-tate1993-main.pdf"), width = 4.5, height = 2.8)

temp <- table_to_tibble(tab_anch)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-tate1993-anchor.pdf"), width = 4.5, height = 2.8)

## De-Contaminating ============================================================
### Correct answers to anchor question -----------------------------------------
### So, who got it "right", anyway?
N <- nrow(tate1993)
sum(tate1993$anc_tate1993 == "123") / N ## Yikes, only 58.0%
correct <- ifelse(tate1993$anc_tate1993 == "123", 1, 0)

### Bootstrapping prep ---------------------------------------------------------
set.seed(12345)
bootstrap_n <- 1000

indices <- seq(bootstrap_n) %>%
  map(
    ~ tibble(
      !!as.name(paste0("x", .x)) :=
        sample(seq(N), size = N, replace = TRUE)
    )
  ) %>%
  bind_cols()
assert_that(!identical(sort(indices$x1), sort(indices$x2)))

### Naive and de-contaminated estimators of pi ---------------------------------
### Naive estimator 1: heroic assumption of zero non-sincere responses
est_naive_bootstrap <- seq(bootstrap_n) %>%
  map(~ table(tate1993$app_tate1993[unlist(indices[, .x])]) / N)

## Unbiased estimation of the proportion of sincere responses (Eq. 9)
est_p_z1_bootstrap <- seq(bootstrap_n) %>%
  map(
    ~ (mean(correct[unlist(indices[, .x])]) - (1 / factorial(3))) /
      (1 - (1 / factorial(3)))
  )

## Check that this is a valid probability
assert_that(all(est_p_z1_bootstrap >= 0))
assert_that(all(est_p_z1_bootstrap <= 1))

## Unbiased estimator for the underlying non-sincere dist. in anchor q. (Eq. 10)
est_pi_nonsincere_bootstrap <- seq(bootstrap_n) %>%
  map(
    ~ (
      permn_augment(
        table(tate1993$anc_tate1993[unlist(indices[, .x])]),
        J = 3
      ) / N -
        (est_p_z1_bootstrap[[.x]]) * c(1, 0, 0, 0, 0, 0)
    ) / (1 - est_p_z1_bootstrap[[.x]])
  )

## Sanity checks
## assert_that(all(est_pi_nonsincere_bootstrap %>% map(sum) %>% unlist() == 1))
## Ah, some precision issue
assert_that(all(near(est_pi_nonsincere_bootstrap %>% map(sum) %>% unlist(), 1)))
assert_that(all(est_pi_nonsincere_bootstrap %>% map(min) %>% unlist() >= 0))
assert_that(all(est_pi_nonsincere_bootstrap %>% map(max) %>% unlist() <= 1))

## De-contaminated ranking data
est_pi_bootstrap <- seq(bootstrap_n) %>%
  map(
    ~ (
      permn_augment(est_naive_bootstrap[[.x]], J = 3) -
        (1 - est_p_z1_bootstrap[[.x]]) * est_pi_nonsincere_bootstrap[[.x]]
    ) /
      est_p_z1_bootstrap[[.x]]
  )

## Welp.
## assert_that(all(est_pi_bootstrap %>% map(sum) %>% unlist() == 1))
assert_that(all(near(est_pi_bootstrap %>% map(sum) %>% unlist(), 1)))
## assert_that(all(est_pi_bootstrap %>% map(min) %>% unlist() >= 0)) --> false
assert_that(all(est_pi_bootstrap %>% map(max) %>% unlist() <= 1))

## Recovering reference rankings from respondents who got the right answer
correct_answer_bootstrap <- seq(bootstrap_n) %>%
  ## No recov_ref_ranking necessary, as Qualtrics has done that step already
  map(
    ~ tate1993$app_tate1993[unlist(indices[, .x])][
      correct[unlist(indices[, .x])] == 1
    ]
  )

## Naive estimator 2: simply use Rs with correct responses to anchor question
est_correct_bootstrap <- seq(bootstrap_n) %>%
  map(
    ~ table(correct_answer_bootstrap[[.x]]) /
      length(correct_answer_bootstrap[[.x]])
  )

### Combine data ---------------------------------------------------------------
ggdat_bootstrap <- seq(bootstrap_n) %>%
  map(
    ~ rbind(
      est_naive_bootstrap[[.x]], est_correct_bootstrap[[.x]],
      est_pi_bootstrap[[.x]]
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

### Calculate standard error ---------------------------------------------------
### For each row, est + ranking are the same
summ_dat <- seq(nrow(ggdat_bootstrap[[1]])) %>%
  map(
    ~ {
      vec <- ggdat_bootstrap %>%
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
  bind_cols(ggdat_bootstrap[[1]] %>% select(-proportion), .) %>%
  mutate(se = ifelse(est == "Quantity of Interest", NA, se))

### Visualization --------------------------------------------------------------
p <- ggplot(summ_dat, aes(x = ranking, y = avg, fill = est)) +
  geom_bar(stat = "identity", position = "dodge2", alpha = 0.7) +
  geom_errorbar(
    position = position_dodge(width = 0.9), width = 0.25,
    aes(ymin = low, ymax = upp)
  ) +
  scale_fill_manual(values = c("gray70", "gray10", "firebrick4")) +
  xlab("") +
  ylab("") +
  scale_y_continuous(
    limits = c(-0.3, 0.9), labels = scales::percent,
    breaks = seq(-0.4, 0.8, by = 0.2)
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.margin = margin(0.2, 0.2, 0, -0.2, "cm")
  )
plot_notitle(pdf_default(p))

ggsave(
  here("fig", "application_tate1993_bootstrapped_pi.pdf"),
  width = 5.5, height = 4
)
