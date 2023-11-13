source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
# remotes::install_github("sysilviakim/Kmisc")

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


# Run Unfolding (there is no way to incorporate weights..., so I improvised)
library(smacof)

set.seed(142)
out <- unfolding(dt[, 1:4], type = "ordinal", circle = "column")
out_w <- unfolding(rank_sample, type = "ordinal", circle = "column")

# Vector Model of Unfolding (Principal Component Analysis for Ranking Data)
set.seed(142)
pca <- vmu(dt[, 1:4])
pca_w <- vmu(rank_sample)


pdf(here::here("fig", "weighting_unfolding.pdf"), width = 6, height = 6)
par(mfrow = c(1, 1))
plot(pca_w,
     col = c("darkcyan", alpha("gray80", 0.1)),
     cex = c(1, 0.7), xlim = c(-1.5, 1.5),
     xlab = "Dimension 1",
     ylab = "Dimension 2"
)
abline(v = 0, lty = 2, col = alpha("black", 0.4))
abline(h = 0, lty = 2, col = alpha("black", 0.4))
dev.off()





pdf(here::here("fig", "weighting_unfolding.pdf"), width = 9.5, height = 10)
par(mfrow = c(2, 2))
plot(out,
  ylim = c(-1, 1),
  main = "Raw Data",
  type = "p", pch = 16, col.columns = "darkred", cex = 1.5,
  label.conf.columns = list(label = TRUE, pos = 3, col = "darkred", cex = 1.2),
  col.rows = 8,
  label.conf.rows = list(label = F, pos = 3, col = alpha("gray80", 0.4))
)
abline(v = 0, lty = 2, col = alpha("gray80", 0.4))
abline(h = 0, lty = 2, col = alpha("gray80", 0.4))

plot(out_w,
  ylim = c(-1, 1),
  main = "With Bias Correction",
  type = "p", pch = 16, col.columns = "darkcyan", cex = 1.5,
  label.conf.columns = list(label = TRUE, pos = 3, col = "darkcyan", cex = 1.2),
  col.rows = 8,
  label.conf.rows = list(label = F, pos = 3, col = alpha("gray80", 0.4))
)
abline(v = 0, lty = 2, col = alpha("gray80", 0.4))
abline(h = 0, lty = 2, col = alpha("gray80", 0.4))


plot(pca,
  col = c("darkred", alpha("gray80", 0.1)),
  cex = c(1, 0.7), xlim = c(-1.5, 1.5)
)
abline(v = 0, lty = 2, col = alpha("black", 0.4))
abline(h = 0, lty = 2, col = alpha("black", 0.4))

plot(pca_w,
  col = c("darkcyan", alpha("gray80", 0.1)),
  cex = c(1, 0.7), xlim = c(-1.5, 1.5)
)
abline(v = 0, lty = 2, col = alpha("black", 0.4))
abline(h = 0, lty = 2, col = alpha("black", 0.4))
text(x = 1.5, y = 1, labels = "White Man?")
text(x = -1.5, y = 1, labels = "White Woman?")
text(x = 1, y = -0.5, labels = "Minority Man?")
text(x = -1, y = -0.5, labels = "Minority Woman?")

dev.off()

temp <- dt %>% filter(pid7 >= 5)
temp2 <- dt %>% filter(pid7 <= 3)

pca_temp <- vmu(temp[, 1:4])
pca_temp2 <- vmu(temp2[, 1:4])

par(mfrow = c(1, 2))
plot(pca_temp, main = "Republicans", xlim = c(-2, 2), ylim = c(-2, 2))
plot(pca_temp2, main = "Democrats", xlim = c(-2, 2), ylim = c(-2, 2))
