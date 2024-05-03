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
    Rrace = race,
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  rename(bias_weight = est.x.adj) %>%
  dplyr::select(
    party, religion, gender, race, id, ideo7, bias_weight
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
    race = as.integer(substr(., 4, 4))
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
hist(rank_sample$race, main = "Race/ethnicity")

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

plot(jitter(rank_sample$party), jitter(rank_sample$race),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Party", ylab = "Race/ethnicity")
abline(lm(race ~ party, rank_sample), col = "darkred", lwd = 1.5)


plot(jitter(rank_sample$religion), jitter(rank_sample$gender),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Religion", ylab = "Gender")
abline(lm(gender ~ religion, rank_sample), col = "darkred", lwd = 1.5)

plot(jitter(rank_sample$religion), jitter(rank_sample$race),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Religion", ylab = "Race/ethnicity")
abline(lm(race ~ religion, rank_sample), col = "darkred", lwd = 1.5)

plot(jitter(rank_sample$gender), jitter(rank_sample$race),
     col = alpha("black", 0.8), pch = ".",
     xlab = "Gender", ylab = "Race/ethnicity")
abline(lm(race ~ gender, rank_sample), col = "darkred", lwd = 1.5)

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

# Goodness-of-fit
out_w

# Model:               Rectangular smacof 
# Number of subjects:  1081 
# Number of objects:   4 
# Transformation:      ordinalp 
# Conditionality:      matrix 
# 
# Stress-1 value:    0.30832 
# Penalized Stress:  1.479971 
# Number of iterations: 6843 

## Permutation test
test <- permtest(out_w)


## Shepard plot
plot(out_w, "Shepard")

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


# Visualize via ggplot
covar <- main %>%
  filter(!is.na(ideo7)) %>%
  mutate(gender3labeled = case_when(gender3 == 1 ~ "male",
                                    gender3 == 2 ~ "female",
                                    TRUE ~ "others")) %>%
  select(race4labeled, gender3, pid3final)

conf_items <- as.data.frame(out_w$conf.col)
conf_persons <- as.data.frame(out_w$conf.row) 


dt_unfold <- cbind(conf_persons, covar) %>%
  select(D1, D2, pid3final) %>%
  group_by(pid3final) %>%
  mutate(n_within_party = n()) %>%
  ungroup() %>%
  group_by(D1, D2, pid3final) %>%
  mutate(n_unique_rank = n(),
         p_unique_within_party = n_unique_rank/n_within_party) %>%
  ungroup() %>%
  arrange(D1, D2, pid3final) %>%
  distinct(D1, D2, pid3final, p_unique_within_party)


# For annotation
anno <- dt_unfold %>%
  filter(D1 > 0 & D1 < 0.25 & D2 > -0.25 & D2 < 0.25)

library(ggrepel)


ggplot(dt_unfold, aes(x = D1, y = D2)) +
  geom_point(aes(shape = pid3final, color = pid3final, size = p_unique_within_party), 
             alpha = 0.5, position = position_dodge(width = 0.15)) + 
  scale_color_manual(breaks = c("Democrat", "Independent", "Republican"),
                     values=c("darkcyan", "purple4", "deeppink4")) +  
  scale_shape_manual(breaks = c("Democrat", "Independent", "Republican"),
                     values = c(1, 2, 0)
  ) +  
  coord_fixed() + 
  geom_point(aes(x = D1, y = D2), conf_items, colour = "black", size = 3) + 
  geom_text(aes(x = D1, y = D2, label = rownames(conf_items)), 
            conf_items, colour = "black", vjust = -0.8, hjust = 0.5) +
  annotate("text", 
           x = unique(anno$D1)-0.1, 
           y = (unique(anno$D2)+0.05), label = "Dem", col = "darkcyan", size = 2) +  
  annotate("text", 
           x = unique(anno$D1), 
           y = (unique(anno$D2)-0.05), label = "Ind", col = "purple4", size = 2) +  
  annotate("text", 
           x = unique(anno$D1)+0.1, 
           y = (unique(anno$D2)+0.05), label = "Rep", col = "deeppink4", size = 2) +    
  xlab("Primary dimension") +
  ylab("Secondary dimension") +
  xlim(-1, 1) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title=element_blank())

ggsave(here::here("fig/sub", "sub_unfolding.pdf"), width = 5, height = 5)
# --> No clear-cut relationship, relative partisanship offers something new



# 4/21/2024, Yuki hides the bias-corrected result because it does not allow us to 
# plot covariates (e.g., race) in the plot

# # Visualize via ggplot
# conf_items <- as.data.frame(out$conf.col)
# conf_persons <- as.data.frame(out$conf.row)
# p2 <- ggplot(conf_persons, aes(x = D1, y = D2)) 
# p2 + geom_jitter(size = 1.5, colour = "gray", alpha = 0.2, width = 0.1, height =  0.1) + 
#   coord_fixed() + 
#   geom_point(aes(x = D1, y = D2), conf_items, colour = "darkred", size = 3) + 
#   geom_text(aes(x = D1, y = D2, label = rownames(conf_items)), 
#             conf_items, colour = "darkred", vjust = -0.8, hjust = 1) +
#   xlab("Dimension 1") +
#   ylab("Dimension 2") +
#   xlim(-1, 1) +
#   ylim(-0.6, 1) +
#   theme_bw() -> p2
# 
# ggpubr::ggarrange(p + ggtitle("Bias Corrected") + 
#                     geom_vline(xintercept = 0, lty = 2, color = "dimgray") +
#                     geom_hline(yintercept = 0, lty = 2, color = "dimgray") +
#                     xlim(-1, 1) +
#                     ylim(-0.6, 1),  
#                   p2 + ggtitle("Raw Data") +
#                     geom_vline(xintercept = 0, lty = 2, color = "dimgray") +
#                     geom_hline(yintercept = 0, lty = 2, color = "dimgray"))
# 
# ggsave(here::here("fig", "weighting_unfolding-compare.pdf"), width = 8, height = 4)


# Figures for Talks
p2 + ggtitle("Raw Data") +
  xlab("Primary Dimension") +
  ylab("Secondary Dimension") +
  xlim(-1, 1) + ylim(-0.7,1)
ggsave(here::here("fig", "weighting_unfolding-raw.pdf"), width = 4, height = 4)

p + ggtitle("Bias Corrected") +
  xlab("Primary Dimension") +
  ylab("Secondary Dimension") +
  xlim(-1, 1) + ylim(-0.7,1)  
ggsave(here::here("fig", "weighting_unfolding-corrected.pdf"), width = 4, height = 4)


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