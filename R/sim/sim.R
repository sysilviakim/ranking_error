# Script Description ===========================================================
# sim.R
# Aim: simulate effects of non-sincere responses with various true distributions
source(here::here("R", "utilities.R"))
set.seed(123)

# Simulation Parameters + Sample Space =========================================
## The below are parameters manipulable for Monte Carlo experiments
N <- 2000 # Number of units
J <- 3 # Number of items
p_z1 <- 0.5 # Proportion of sincere responses
pi_geometric <- c(0.05, 0.3, 0.15, 0.15, 0.3, 0.05) 
## Non-sincere response pattern; prob dist. over rankings (hypothetical zig-zag)
## If we wanted uniform patterns
## pi_geometric <- rep(1 / length(perm), length(perm))

## Generate the population ranking distribution --------------------------------
## V1 = first choice, V2 = second choice, V3 = third choice, ...
prob_vec_list <- list(
  ## No individually unique preference; all preferences are the same
  homogeneous = rep(1 / J, J),
  ## Using uniformly distributed first-choice probability (indifferent!)
  uniform = rep(1 / J, J),
  ## Instead of uniform dist., deliberate create a nonuniform dist.
  ## If J = 3, 0.2, 0.3, 0.5
  skewed_01 = rev(
    c(0.5, seq(0.3, 0.2, length.out = J - 1) /
      sum(seq(0.3, 0.2, length.out = J - 1)) / 2)
  ),
  ## If J = 3, 0.7, 0.2, 0.1
  skewed_02 = c(0.7, seq(0.2, 0.1, length.out = J - 1)) /
    sum(c(0.7, seq(0.2, 0.1, length.out = J - 1))),
  anchor = rep(1 / J, J)
)

## Store chi-squared test for discrete uniform check
chisq_list <- rep(
  list(list(
    true_permn = NA, random_permn = NA,
    obs_sincere = NA, obs_geometric = NA, obs_half = NA
  )),
  length = length(prob_vec_list)
)

## In addition, observed data patterns for diff. conditions
obs_data_list <- permn_list <- vector("list", length = length(prob_vec_list))
names(obs_data_list) <- names(permn_list) <- names(chisq_list) <-
  names(prob_vec_list)

# Loop: True and Observed Rankings =============================================
for (scenario in names(prob_vec_list)) {
  prob_vec <- prob_vec_list[[scenario]]
  true_pref <- rpluce(n = N, t = J, prob = prob_vec)

  if (scenario == "homogeneous") {
    true_pref <- bind_rows(
      ## this is to secure at least one obs of all unique patterns
      ## and avoid problems in plotting (cheap workaround)
      true_pref %>% dedup(),
      tibble(
        `1st` = rep("c", N),
        `2nd` = rep("b", N),
        `3rd` = rep("a", N)
      )
    ) %>%
      slice(1:N)
  }
  
  if (scenario == "anchor") {
    ## Creating the "correct" response in the anchor question: a-b-c
    true_pref <- tibble(
      `1st` = rep("a", N),
      `2nd` = rep("b", N),
      `3rd` = rep("c", N)
    )
  }

  ## Generate true rankings ----------------------------------------------------
  ## Note: if (3, 2, 1), A corresponds to 3, B to 2, and C to 1.
  true_rank <- item_to_rank(true_pref)
  true_rank

  ## True rank squashed into J! permutation pattern strings
  true_permn <- true_rank %>% unite(order, sep = "")
  table(true_permn)
  if (scenario != "homogeneous" & scenario != "anchor") {
    chisq_list[[scenario]]$true_permn <- chisq.test(table(true_permn))
  }

  # Survey's question choice order ---------------------------------------------
  ## Assumption 1: Natural order A-B-C, simply observe true permutation
  ## Assumption 2: Order of items randomized
  random_choices <- rpluce(n = N, t = J, prob = rep(1 / J, J))
  head(random_choices)

  ## Check the uniformity: proportions + pearson's chi-squared test
  random_permn <- random_choices %>% unite(order, sep = "")
  prop_vector(random_permn)
  chisq_list[[scenario]]$random_permn <- chisq.test(table(random_permn))

  # Generate respondents' "stated rankings" ------------------------------------

  ## Under random order of choices
  ## (D) 100% sincere respondents
  obs_sincere <- loop_stated_rank_preference(true_rank, random_choices)
  prop_vector(obs_sincere)
  chisq_list[[scenario]]$obs_sincere <- chisq.test(table(obs_sincere))
  ## Proposition: the dist of observed rankings will be uniform

  ## (B) 0% sincere respondents
  ## Note: These units only rank according to patterns, not based on preference
  ## Assuming uniform random patterns, get all elements in the sample space
  perm <- combinat::permn(x = 1:J) %>%
    map_chr(~ paste(.x, collapse = "")) %>%
    sort()

  obs_geometric <- 
    sample(x = perm, size = N, replace = TRUE, prob = pi_geometric)
  obs_geometric <- tibble(obs_rank = obs_geometric)

  head(obs_geometric)
  prop_vector(obs_geometric)
  chisq_list[[scenario]]$obs_geometric <- chisq.test(table(obs_geometric))
  ## Proposition: this will never be uniform; p-val must be < 0.001

  ## (C) 50% sincere respondents (fixed order (a, b, c)) if p_z1 = 0.5
  fixed_half <- rbind(
    true_permn[1:floor(N * p_z1), ] %>% rename(obs_rank = order),
    obs_geometric[(floor(N * p_z1) + 1):N, ]
  )

  ### (F) 50% sincere respondents (item order randomization)
  obs_half <- rbind(
    obs_sincere[1:floor(N * p_z1), ],
    obs_geometric[(floor(N * p_z1) + 1):N, ]
  )
  prop_vector(obs_half)
  chisq_list[[scenario]]$obs_half <- chisq.test(table(obs_half))

  ## Generating observed rank-order question
  obs_data <- bind_cols(
    obs_sincere, ## sincere responses
    obs_geometric %>% rename(obs_geometric = obs_rank), ## non-sincere resp.
    random_choices, ## observed choice set
    true_rank,
    true_permn %>% rename(true_permn = order)
  )
  
  obs_data_list[[scenario]] <- obs_data

  # Visualize: compare observed patterns ---------------------------------------
  pdf(
    here("fig", "sim", paste0("sim_obs_ranking_", scenario, ".pdf")),
    width = 7, height = 5
  )
  permn_list[[scenario]] <- list(
    ## fixed_100p: if 100% sincere, observe true permutation
    p1_fixed_100p = true_permn %>%
      rename(obs_rank = order),
    ## fixed_0p: if 0% sincere, always observe zigzag
    p2_fixed_0p = obs_geometric,
    ## fixed_50p: mixture
    p3_fixed_50p = fixed_half,
    ## rand_100p: if 100% sincere, this is the observed pattern
    p4_rand_100p = obs_sincere,
    ## rand_0p: if 0% sincere, regardless of randomization, zigzag
    p5_rand_0p = obs_geometric,
    ## rand_50p: mixture
    p6_rand_50p = obs_half
  ) %>%
    map(rowid)

  p_list <- permn_list[[scenario]] %>%
    imap(
      ~ {
        p <- ggplot(.x) +
          geom_bar(
            aes(x = obs_rank, y = ..count.. / sum(..count..)),
            fill = "deepskyblue3"
          ) +
          xlab("") +
          scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          geom_hline(yintercept = 1 / 6)

        ## y-axis label
        if (.y == "p4_rand_100p") {
          p <- p + ylab("Item Order Randomization")
        } else if (.y == "p1_fixed_100p") {
          p <- p + ylab("Reference Item Set (a, b, c)")
        } else {
          p <- p + ylab("")
        }

        return(pdf_default(p))
      }
    )

  p_list[[1]] <- p_list[[1]] + ggtitle("A. 100% Sincere")
  p_list[[2]] <- p_list[[2]] + ggtitle("B. 0% Sincere")
  p_list[[3]] <- p_list[[3]] + ggtitle("C. 50% Sincere")
  p_list[[4]] <- p_list[[4]] + ggtitle("D. 100% Sincere")
  p_list[[5]] <- p_list[[5]] + ggtitle("E. 0% Sincere")
  p_list[[6]] <- p_list[[6]] + ggtitle("F. 50% Sincere")

  p <- p_list %>%
    map(~ .x + theme(plot.title = element_text(size = 10))) %>%
    wrap_plots(ncol = 3)
  print(p)
  dev.off()
}

# Export =======================================================================
save(permn_list, file = here("output", "sim_permn_list.Rda"))
save(chisq_list, file = here("output", "sim_chisq_list.Rda"))

# Average ranking contamination proof ==========================================
permn_list$homogeneous %>%
  map(~ avg_rank(.x, "obs_rank")) %>%
  bind_rows(.id = "pane")
