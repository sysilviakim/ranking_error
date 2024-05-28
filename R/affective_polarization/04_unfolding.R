source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
## source(here::here("R", "imprr_weights.R")) --> no need to call separately
## now part of the ranking package

# Grab main data
main <- df_list$main

# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)
# Grab rankings and weights

# Get bias-correction weights ==================================================

data <- main %>%
  select(app_polar_1, app_polar_2, app_polar_3, app_polar_4, app_polar_5,
         anc_polar_1, anc_polar_2, anc_polar_3, anc_polar_4, app_polar_5,
         anc_correct_polar)

w <- imprr_weights(
  data = data,
  J = 5,
  main_q = "app_polar",
  anchor_q =  "anc_polar",
  anc_correct = "anc_correct_polar"
)

print(w)


# Bias-corrected PMF
w$corrected_pmf %>%
  select(ranking, prop_renormalized) %>%
  arrange(desc(prop_renormalized)) %>%
  xtable()




# Downsize data
dt <- main %>%
  mutate(id = 1:nrow(main)) %>%
  rename(
    politician = app_polar_1,
    media = app_polar_2,
    social = app_polar_3,
    interest = app_polar_4,
    citizen = app_polar_5,
    ranking = app_polar
  ) %>%
  left_join(w$weights, by = "ranking") %>%
  dplyr::select(
    politician, media, social, interest, citizen, w
  ) 

# Sampling from correct PMF
set.seed(142)
rank_sample <- NA

# In the following,
## 1: Draw one value from a multinomial distribution
## here, bias-corrected PMFs are the probability vector
## here, we draw a given ranking whose value is 1
## 2: Store the value in the i-th position of "rank_sample"

for (i in 1:1082) {
  rank_sample[i] <-
    w$weights$ranking[rmultinom(1, size = 1, prob = w$weights$w) == 1]
}


# Something went wrong, so I am putting a hold of using this for now (Yuki)
dt_correct <- rank_sample %>%
  tibble() %>%
  mutate(
    # politician = unlist(gregexpr('1', .)), # Get position in which "1" appears
    # media = unlist(gregexpr('2', .)),
    # social = unlist(gregexpr('3', .)),
    # interest = unlist(gregexpr('4', .)),
    # citizen = unlist(gregexpr('5', .))
        politician = as.integer(substr(., 1, 1)),
        media = as.integer(substr(., 2, 2)),
        social = as.integer(substr(., 3, 3)),
        interest = as.integer(substr(., 4, 4)),
        citizen = as.integer(substr(., 5, 5))
  ) 

par(mfrow = c(1,2))
hist(main$anc_polar_5)
hist(dt_correct$citizen)

sort(prop.table(table(dt_correct$.)))

# Why is 5 (marignal rank) less prevalent in rank_sample?
# --> Need to solve this issue. I may have made a mistake inside "mutate"


# Run Unfolding ================================================================

library(smacof)

set.seed(142)
out <- unfolding(dt[, 1:5], type = "ordinal", circle = "column")
#out_w <- unfolding(dt_correct, type = "ordinal", circle = "column")

# Goodness-of-fit
out

# Model:               Rectangular smacof 
# Number of subjects:  1081 
# Number of objects:   4 
# Transformation:      ordinalp 
# Conditionality:      matrix 
# 
# Stress-1 value:    0.30832 
# Penalized Stress:  1.479971 
# Number of iterations: 6843 

# ## Permutation test (Supressed now, it takes a while)
# test <- permtest(out)


## Shepard plot
plot(out, "Shepard")

# If the MDS solution were perfect, then all open circles in the Shepard diagram in
# Fig. 3.1 would lie on the monotonic regression line, because then all data would be
# mapped into distances that are (weakly) ordered as the data. This is obviously not
# true here. We can measure “how untrue” it is by considering the squared difference
# of each disparity and its MDS distance.

## Vector Methods of Unfolding (>83% explained)
vmu_w <- vmu(rank_sample)
vmu_w

# Call: vmu(delta = rank_sample)
# 
# Number of subjects: 1081
# Number of objects: 4
# Number of dimensions: 2
# Variance accounted for: 83.13%

conf_items <- as.data.frame(out$conf.col)
conf_persons <- as.data.frame(out$conf.row) 



# Single graph
set.seed(24)
conf_persons %>%
  ggplot(aes(x = D1, y = D2)) +
  geom_jitter(width = 0.05, height = 0.05,
              shape = 1, 
              color ="darkcyan", 
              alpha = 0.3) + 
  geom_point(aes(x = D1, y = D2), conf_items, colour = "black", size = 3) + 
  geom_text(aes(x = D1, y = D2, label = rownames(conf_items)), 
            conf_items, colour = "black", vjust = -0.8, hjust = 0.5) +    
  xlab("Primary dimension") +
  ylab("Secondary dimension") +
  xlim(-1, 1) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank())


ggsave(here::here("fig", "unfolding_polar.pdf"), 
       width = 6*1.1, height = 6)


