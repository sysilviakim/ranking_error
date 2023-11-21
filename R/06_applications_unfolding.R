source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Grab main data
main <- df_list$main

# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)
# Grab rankings and weights
imp_w <- read_csv(here::here("data/tidy", "temp_weight.csv")) %>%
  mutate(
    app_identity = as.character(ranking),
    bias_weight = weight
  ) %>%
  dplyr::select(app_identity, est.x.adj)


# Downsize data
dt <- main %>%
  mutate(id = 1:nrow(main)) %>%
  rename(
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race_ethnicity = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  rename(bias_weight = est.x.adj) %>%
  dplyr::select(
    party, religion, gender, race_ethnicity, id, ideo7, bias_weight
  ) %>%
  filter(!is.na(ideo7))

# "sample" from correct PMF
set.seed(142)
rank_sample <- NA
for (i in 1:1081) {
  rank_sample[i] <-
    imp_w$app_identity[rmultinom(1, size = 1, prob = imp_w$est.x.adj) == 1]
}

rank_sample <- rank_sample %>%
  tibble() %>%
  mutate(
    party = as.integer(substr(., 1, 1)),
    religion = as.integer(substr(., 2, 2)),
    gender = as.integer(substr(., 3, 3)),
    race_ethnicity = as.integer(substr(., 4, 4))
  ) %>%
  dplyr::select(-.)


# Motivate Unfolding ===========================================================
# Now, let's understand which items are similar (or dissimilar) to each other
# The most intuitive analysis is to see a binariate relationship between a pair
# of items

# First, let's see the univariate distribution of each item separately
par(mfrow = c(2,2))
hist(rank_sample$party, main = "Party")
hist(rank_sample$religion, main = "Religion")
hist(rank_sample$gender, main = "Gender")
hist(rank_sample$race_ethnicity, main = "Race/ethnicity")

## This is very interesting in and itself. Religion has a non-single peaked pref

# Second, let's see the bivariate relationship of each pair of items
par(mfrow = c(2,3),
    mar = c(4.5,4,3,2))
plot(jitter(rank_sample$party), jitter(rank_sample$religion),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Party", ylab = "Religion")
abline(lm(religion ~ party, rank_sample), col = "darkred", lwd = 1.5)

plot(jitter(rank_sample$party), jitter(rank_sample$gender),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Party", ylab = "Gender")
abline(lm(gender ~ party, rank_sample), col = "darkred", lwd = 1.5)

plot(jitter(rank_sample$party), jitter(rank_sample$race_ethnicity),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Party", ylab = "Race/ethnicity")
abline(lm(race_ethnicity ~ party, rank_sample), col = "darkred", lwd = 1.5)


plot(jitter(rank_sample$religion), jitter(rank_sample$gender),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Religion", ylab = "Gender")
abline(lm(gender ~ religion, rank_sample), col = "darkred", lwd = 1.5)

plot(jitter(rank_sample$religion), jitter(rank_sample$race_ethnicity),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Religion", ylab = "Race/ethnicity")
abline(lm(race_ethnicity ~ religion, rank_sample), col = "darkred", lwd = 1.5)

plot(jitter(rank_sample$gender), jitter(rank_sample$race_ethnicity),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Gender", ylab = "Race/ethnicity")
abline(lm(race_ethnicity ~ gender, rank_sample), col = "darkred", lwd = 1.5)

# We can see that many items have negative correlations
# Exceptions are party-gender and gender-race

# Now, can we examine the tri-variate relations?
# The answer is probably NO since we are running out of dimensions
# Instead of creating 3-D plots, we seek to squash the dimensions and 
# plot the results in a two-dimensional space
# That's the main idea for unfolding (multidimensional scaling, PCA, all similar)


# Run Unfolding ================================================================
# (there is no way to incorporate weights..., so I improvised)
library(smacof)

set.seed(142)
out <- unfolding(dt[, 1:4], type = "ordinal", circle = "column")
out_w <- unfolding(rank_sample, type = "ordinal", circle = "column")

# Visualize via ggplot
conf_items <- as.data.frame(out_w$conf.col)
conf_persons <- as.data.frame(out_w$conf.row)
p <- ggplot(conf_persons, aes(x = D1, y = D2)) 
p + geom_jitter(size = 1.5, colour = "gray", alpha = 0.2, width = 0.1, height =  0.1) + 
  coord_fixed() + 
  geom_point(aes(x = D1, y = D2), conf_items, colour = "darkcyan", size = 3) + 
  geom_text(aes(x = D1, y = D2, label = rownames(conf_items)), 
            conf_items, colour = "darkcyan", vjust = -0.8, hjust = 1) +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  theme_bw() -> p

ggsave(here::here("fig", "weighting_unfolding.pdf"), width = 4, height = 4)



# Visualize via ggplot
conf_items <- as.data.frame(out$conf.col)
conf_persons <- as.data.frame(out$conf.row)
p2 <- ggplot(conf_persons, aes(x = D1, y = D2)) 
p2 + geom_jitter(size = 1.5, colour = "gray", alpha = 0.2, width = 0.1, height =  0.1) + 
  coord_fixed() + 
  geom_point(aes(x = D1, y = D2), conf_items, colour = "darkred", size = 3) + 
  geom_text(aes(x = D1, y = D2, label = rownames(conf_items)), 
            conf_items, colour = "darkred", vjust = -0.8, hjust = 1) +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  xlim(-1, 1) +
  ylim(-0.6, 1) +
  theme_bw() -> p2

ggpubr::ggarrange(p + ggtitle("Bias Corrected") + 
                    geom_vline(xintercept = 0, lty = 2, color = "dimgray") +
                    geom_hline(yintercept = 0, lty = 2, color = "dimgray") +
                    xlim(-1, 1) +
                    ylim(-0.6, 1),  
                  p2 + ggtitle("Raw Data") +
                    geom_vline(xintercept = 0, lty = 2, color = "dimgray") +
                    geom_hline(yintercept = 0, lty = 2, color = "dimgray"))

ggsave(here::here("fig", "weighting_unfolding-compare.pdf"), width = 8, height = 4)


# # Vector Model of Unfolding (Principal Component Analysis for Ranking Data)
# set.seed(142)
# vmu <- vmu(dt[, 1:4])
# vmu_w <- vmu(rank_sample)
# pdf(here::here("fig", "weighting_unfolding.pdf"), width = 6, height = 6)
# par(mfrow = c(1, 2))
# plot(pca_w,
#      col = c("darkcyan", alpha("gray80", 0.1)),
#      cex = c(1, 0.7), xlim = c(-1.5, 1.5),
#      xlab = "Dimension 1",
#      ylab = "Dimension 2"
# )
# abline(v = 0, lty = 2, col = alpha("black", 0.4))
# abline(h = 0, lty = 2, col = alpha("black", 0.4))
# dev.off()