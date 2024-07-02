source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Subset and wrangle data that we need =========================================
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

## Reference set: (party, religion, gender, race)
identity_data <- main %>%
  mutate(pid_recode = case_when(pid3 == 1 ~ "Democrat",
                                pid3 == 2 ~ "Republican",
                                TRUE ~ "Others")) %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity, anc_correct_id_alphabet, anc_correct_id_exact,
    weight,
    pid_recode
  )


# Stratification by bootstrap  =================================================

set.seed(NULL)
seed_strat <- 1245
n_bootstrap <- 200
out_stratification <- data.frame(mean = as.numeric(),
                                 item = as.character())

# set.seed(seed_strat) Using random seed here fixes all bootstrap estimates
for (b in 1:n_bootstrap) {
## Sample indices
index <- sample(1:nrow(identity_data), size = nrow(identity_data), replace = TRUE)
  
## This is the bootstrapped data
boostrap_dat <- identity_data [index, ]

## Estimated proportions of strata
p_X <- prop.table(table(boostrap_dat$pid_recode))
  
## Stratify by partisanship
data_dem <- boostrap_dat %>% filter(pid_recode == "Democrat")
data_rep <- boostrap_dat %>% filter(pid_recode == "Republican")
data_oth <- boostrap_dat %>% filter(pid_recode == "Others")

# Apply bias correction ========================================================
## Democrat --------------------------------------------------------------------
direct_dem <- imprr_direct(
  data = data_dem,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data_dem$weight,
  n_bootstrap = 1
)

## Republican ------------------------------------------------------------------
direct_rep <- imprr_direct(
  data = data_rep,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data_rep$weight,
  n_bootstrap = 1
)

## Others ----------------------------------------------------------------------
direct_oth <- imprr_direct(
  data = data_oth,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data_oth$weight,
  n_bootstrap = 1
)

# Stratification estimate ======================================================

## Stratification estimate
est_dem <- direct_dem$qoi %>% filter(qoi == "average rank") %>% ungroup() %>% select(item, mean)
est_rep <- direct_rep$qoi %>% filter(qoi == "average rank") %>% ungroup() %>% select(item, mean)
est_oth <- direct_oth$qoi %>% filter(qoi == "average rank") %>% ungroup() %>% select(item, mean)

strat <- 
est_dem[1:4, 2] * p_X[1] + 
  est_rep[1:4, 2] * p_X[2] + 
  est_oth[1:4, 2] * p_X[3] 

strat$item <- c("party", "religion", "gender", "race_ethnicity")

out_stratification <- rbind(out_stratification, strat)
}

avg_gg_comb <- out_stratification %>%
               group_by(item) %>%
               summarize(estimate = mean(mean),
               conf.low = quantile(mean, prob = 0.025),
               conf.high = quantile(mean, prob = 0.975))

# Visualization ================================================================

width_par <- 0.5

p <- avg_gg_comb %>%
  mutate(
    item = case_when(
      item == "party" ~ "Party",
      item == "religion" ~ "Religion",
      item == "race_ethnicity" ~ "Race/ethnicity",
      item == "gender" ~ "Gender"
    )
  ) %>%
  ggplot(
    aes(
      y = fct_reorder(item, -estimate, mean),
      x = estimate
    )
  ) +
  scale_fill_brewer(palette = "Accent") +
  geom_vline(
    xintercept = 2.5, lty = 2, color = alpha("black", 0.5), linewidth = 0.3
  ) +
  geom_point(
    position = position_dodge(width = width_par), size = 1.5
  ) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0,
                position = position_dodge(width_par), size = 0.6
  ) +
  scale_color_manual(
    values = c(
      "Direct" = "darkcyan",
      "IPW" = "maroon4",
      "Raw Data" = alpha("dimgray", 0.5)
    )
  ) +
  geom_text(
    aes(
      x = conf.high + 0.15,
      label = round(estimate, 1.5)
    ),
    position = position_dodge(width = width_par),
    size = 2,
    color = "black",
    family = "CM Roman"
  ) +
  xlim(1, 4.1) +
  ylab("") +
  xlab("") +
  theme_classic(base_rect_size = 11 / 44)

pdf_default(p) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    text = element_text(size = 8)
  )
ggsave(
  here("fig", "weight-avg-rank-theta-stratification.pdf"),
  width = 4.5, height = 3
)
