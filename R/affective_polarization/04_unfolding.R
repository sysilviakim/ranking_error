source(here::here("R/affective_polarization", "01_setup.R"))

# Data load and setup ==========================================================
load(here("output", "polar.Rda"))
main <- main %>%
  left_join(ipw_out$weights, by = c("app_polar" = "ranking"))

# Bias-corrected PMF ===========================================================
ipw_out$corrected_pmf %>%
  select(ranking, prop_renormalized) %>%
  arrange(desc(prop_renormalized)) %>%
  xtable()

# Downsize data
dt <- main %>%
  mutate(id = row_number()) %>%
  select(
    rank_politicians, rank_media, rank_social, rank_interest, rank_citizens, w
  )

# Sampling from correct PMF
set.seed(142)
rank_sample <- NA

# In the following,
## 1: Draw one value from a multinomial distribution
## here, bias-corrected PMFs are the probability vector
## here, we draw a given ranking whose value is 1
## 2: Store the value in the i-th position of "rank_sample"

for (i in seq(nrow(main))) {
  rank_sample[i] <-
    ipw_out$weights$ranking[
      rmultinom(1, size = 1, prob = ipw_out$weights$w) == 1]
}

# Something went wrong, so I am putting a hold of using this for now (Yuki)
# dt_correct <- rank_sample %>%
#   tibble() %>%
#   mutate(
#     politician = as.integer(substr(., 1, 1)),
#     media = as.integer(substr(., 2, 2)),
#     social = as.integer(substr(., 3, 3)),
#     interest = as.integer(substr(., 4, 4)),
#     citizen = as.integer(substr(., 5, 5))
#   )
# 
# par(mfrow = c(1, 2))
# hist(main$anc_polar_5)
# hist(dt_correct$citizen)
# 
# sort(prop.table(table(dt_correct$.)))

# Run Unfolding ================================================================
library(smacof)
set.seed(142)
out <- unfolding(dt[, 1:5], type = "ordinal", circle = "column")
# out_w <- unfolding(dt_correct, type = "ordinal", circle = "column")

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

# If the MDS solution were perfect, then all open circles in the Shepard 
# diagram in Fig. 3.1 would lie on the monotonic regression line, 
# because then all data would be mapped into distances that are 
# (weakly) ordered as the data. This is obviously not true here. 
# We can measure “how untrue” it is by considering the squared difference
# of each disparity and its MDS distance.

## Vector Methods of Unfolding (>83% explained) --> this code no longer works?
# vmu_w <- vmu(rank_sample)
# vmu_w

# Call: vmu(delta = rank_sample)
#
# Number of subjects: 1081
# Number of objects: 4
# Number of dimensions: 2
# Variance accounted for: 83.13%

conf_items <- as.data.frame(out$conf.col)
rownames(conf_items) <- c(
  "Politicians", "Print Media and TV",
  "Social Media", "Interest Groups", "Citizens"
)
conf_persons <- as.data.frame(out$conf.row)

# Single graph
set.seed(24)
p <- conf_persons %>%
  ggplot(aes(x = D1, y = D2)) +
  geom_jitter(
    width = 0.05, height = 0.05,
    shape = 1,
    color = "darkcyan",
    alpha = 0.3
  ) +
  geom_point(aes(x = D1, y = D2), conf_items, colour = "black", size = 3) +
  geom_text(
    aes(x = D1, y = D2, label = rownames(conf_items)),
    conf_items,
    colour = "black", vjust = -0.8, hjust = 0.5, family = "CM Roman"
  ) +
  xlab("Primary dimension") +
  ylab("Secondary dimension") +
  xlim(-1, 1.1) +
  ylim(-1, 1.1) + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  )
pdf_default(p)
ggsave(here("fig", "unfolding_polar.pdf"), width = 5, height = 5)
