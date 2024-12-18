# Script Description ===========================================================
# Aim: illustrate weights in our de-contamination step
library(scales)
N <- 1000

# Generate Data ================================================================
## Target PMF ------------------------------------------------------------------
pi_s <- c(
  rep(123, N * 0.4),
  rep(131, N * 0.3),
  rep(213, N * 0.15),
  rep(231, N * 0.01),
  rep(312, N * 0.09),
  rep(321, N * 0.05)
)

## Noise PMF -------------------------------------------------------------------
pi_n <- c(
  rep(123, N * 0.167),
  rep(131, N * 0.167),
  rep(213, N * 0.167),
  rep(231, N * 0.167),
  rep(312, N * 0.167),
  rep(321, N * 0.167)
)

## Raw data --------------------------------------------------------------------
raw <- c(pi_s, pi_n)

## Weight ----------------------------------------------------------------------
weight <- (table(pi_s) / length(pi_s)) /
  (table(raw) / length(raw))

## Sanity check
table(pi_s) / length(pi_s)
table(pi_n) / length(pi_n)
table(raw) / length(raw)
weight
sum(weight) # Must sum up to 5 

#       123       131       213       231       312       321
# 1.4123457 1.2860814 0.7498127 0.4612903 0.7498127 0.4612903

# --> 123 is "under-sampled", so we need to "bump it up"
# --> 321 is "over-sampled", so we need to "squash it down"

# Visualization ================================================================
p_raw <- as.data.frame(table(raw) / length(raw))
p_true <- as.data.frame(table(pi_s) / length(pi_s))

## Figure 5, formerly illustrate.pdf
pdf(
  here("fig", "Fig5.pdf"),
  width = 7, height = 5
)

par(mfrow = c(1, 1), mar = c(3, 4, 1, 1))
plot(
  x = 1,
  y = 1,
  las = 1,
  mgp = c(2, 0.7, 0),
  xaxt = "n",
  xlim = c(-0.1, 6.5),
  ylim = c(0, 0.45),
  xlab = "",
  ylab = "Proportion"
)
axis(
  side = 1, at = c(1, 2, 3, 4, 5, 6),
  labels = c("123", "132", "213", "231", "312", "321")
)
points(p_true, col = "darkcyan", cex = 1.5, pch = 16)
lines(p_true, col = "darkcyan", lwd = 2)
points(p_raw, col = "dimgray", cex = 1.5, pch = 16)
lines(p_raw, col = "dimgray", lwd = 2, lty = 2)
text(
  x = 2.7, y = 0.38, labels = "Bias-Corrected Data",
  cex = 1.2, col = "darkcyan", font = 2
)
text(
  x = 1.5, y = 0.18, labels = "Raw Data",
  cex = 1.2, col = "black", font = 2
)
arrows(
  x0 = 1, x1 = 1,
  y0 = p_raw$Freq[1] + 0.01,
  y1 = p_true$Freq[1] - 0.01,
  length = 0.1, lwd = 3, col = alpha("deeppink4", 0.3)
)
arrows(
  x0 = 4, x1 = 4,
  y0 = p_raw$Freq[4] - 0.01,
  y1 = p_true$Freq[4] + 0.01,
  length = 0.1, lwd = 3, col = alpha("deeppink4", 0.3)
)
text(
  x = 0.4, y = 0.35, labels = "needs to be\nscaled up\n weight > 1",
  col = "deeppink4"
)
text(
  x = 4, y = 0.16, labels = "needs to be\nscaled down\n weight < 1",
  col = "deeppink4"
)
rect(
  xleft = 0.6, ybottom = 0, xright = 1.4, ytop = p_raw$Freq[1],
  col = alpha("dimgray", 0.3), lty = 0
)
rect(
  xleft = 1.6, ybottom = 0, xright = 2.4, ytop = p_raw$Freq[2],
  col = alpha("dimgray", 0.3), lty = 0
)
rect(
  xleft = 2.6, ybottom = 0, xright = 3.4, ytop = p_raw$Freq[3],
  col = alpha("dimgray", 0.3), lty = 0
)
rect(
  xleft = 3.6, ybottom = 0, xright = 4.4, ytop = p_raw$Freq[4],
  col = alpha("dimgray", 0.3), lty = 0
)
rect(
  xleft = 4.6, ybottom = 0, xright = 5.4, ytop = p_raw$Freq[5],
  col = alpha("dimgray", 0.3), lty = 0
)
rect(
  xleft = 5.6, ybottom = 0, xright = 6.4, ytop = p_raw$Freq[6],
  col = alpha("dimgray", 0.3), lty = 0
)
dev.off()
