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
prob_vec <- rep(1 / J, J)

## Store chi-squared test for discrete uniform check
chisq_list <- list(
  list(
    true_permn = NA, random_permn = NA,
    obs_sincere = NA, obs_geometric = NA, obs_half = NA
  )
)

## In addition, observed data patterns for diff. conditions
true_pref <- rpluce(n = N, t = J, prob = prob_vec)
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

## Generate true rankings ------------------------------------------------------
## Note: if (3, 2, 1), A corresponds to 3, B to 2, and C to 1.
true_rank <- item_to_rank(true_pref)
true_rank

## True rank squashed into J! permutation pattern strings
true_permn <- true_rank %>% unite(order, sep = "")
table(true_permn)

# Survey's question choice order -----------------------------------------------
## Assumption 1: Natural order A-B-C, simply observe true permutation
## Assumption 2: Order of items randomized
random_choices <- rpluce(n = N, t = J, prob = rep(1 / J, J))
head(random_choices)

## Check the uniformity: proportions + pearson's chi-squared test
random_permn <- random_choices %>% unite(order, sep = "")
prop_vector(random_permn)
chisq_list$random_permn <- chisq.test(table(random_permn))

# Generate respondents' recorded responses -------------------------------------

## Under random order of choices
## (D) 100% sincere respondents
obs_sincere <- loop_stated_rank_preference(true_rank, random_choices)
prop_vector(obs_sincere)
chisq_list$obs_sincere <- chisq.test(table(obs_sincere))
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
chisq_list$obs_geometric <- chisq.test(table(obs_geometric))
## Proposition: this will never be uniform; p-val must be < 0.001

## (C) 50% sincere respondents (fixed order (a, b, c)) if p_z1 = 0.5
fixed_half <- rbind(
  true_permn[1:floor(N * p_z1), "order", drop = FALSE] %>%
    rename(obs_rank = order),
  obs_geometric[(floor(N * p_z1) + 1):N, ]
)

### (F) 50% sincere respondents (item order randomization)
obs_half <- rbind(
  obs_sincere[1:floor(N * p_z1), , drop = FALSE],
  obs_geometric[(floor(N * p_z1) + 1):N, , drop = FALSE]
)
prop_vector(obs_half)
chisq_list$obs_half <- chisq.test(table(obs_half))

## Generating observed rank-order question
obs_data <- bind_cols(
  obs_sincere, ## sincere responses
  obs_geometric %>% rename(obs_geometric = obs_rank), ## non-sincere resp.
  random_choices, ## observed choice set
  true_rank,
  true_permn %>% rename(true_permn = order)
)

# Visualize: compare observed patterns -----------------------------------------
pdf(here("fig", "FigB2.pdf"), width = 7, height = 5)
permn_list <- list(
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

p_list <- permn_list %>%
  imap(
    ~ {
      p <- ggplot(.x) +
        geom_bar(
          aes(x = obs_rank, y = ..count.. / sum(..count..)),
          fill = "firebrick4"
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

p_list[[1]] <- p_list[[1]] + ggtitle("A. 0% Random")
p_list[[2]] <- p_list[[2]] + ggtitle("B. 100% Random")
p_list[[3]] <- p_list[[3]] + ggtitle("C. 50% Random")
p_list[[4]] <- p_list[[4]] + ggtitle("D. 0% Random")
p_list[[5]] <- p_list[[5]] + ggtitle("E. 100% Random")
p_list[[6]] <- p_list[[6]] + ggtitle("F. 50% Random")

p <- p_list %>%
  map(~ pdf_default(.x) + theme(plot.title = element_text(size = 10))) %>%
  wrap_plots(ncol = 3)
print(p)
dev.off()
