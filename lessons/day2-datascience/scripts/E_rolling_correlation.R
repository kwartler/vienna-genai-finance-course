# ============================================================
# E_rolling_correlation.R
# Quantitative Extension lesson, Day 2, Data Science Masters
#
# Purpose: compute rolling pairwise correlations between the
# five equities from A_data_prep.R over two window lengths
# (30 and 90 trading days) and visualize how those
# relationships shift over time. Run A_data_prep.R first.
# ============================================================

# ---- Libraries ----
# TTR: runCor gives a rolling correlation between two series
#      (the same package the Day 1 indicator work used)
# xts: keeps the date index attached through the calculation
#      (loads zoo, which we use for plotting)
library(TTR)
library(xts)

# ---- Load data from A_data_prep.R ----
loadPth     <- '~/Desktop/vienna-genai-finance-course/portfolio_files'
returns_xts <- readRDS(file.path(loadPth, "returns_xts.rds"))
tickers     <- colnames(returns_xts)

# ---- Windows (trading days, not calendar days) ----
SHORT_WINDOW <- 30   # about 6 calendar weeks
LONG_WINDOW  <- 90   # about 4.5 calendar months
# Shorter window: more reactive, but noisier. Longer window:
# smoother, but slower to reveal a real change. You will see
# both effects in the overlay plot near the bottom.

# ---- Guard: enough history for the long window? ----
if (nrow(returns_xts) < LONG_WINDOW) {
  stop("Not enough return history for the long window. Increase LOOKBACK_YEARS in A_data_prep.R.")
} else {
  cat("History length is fine for both windows.\n")
}

# ---- All unique pairs of tickers ----
# combn lists every 2-way combination. With 5 tickers that is
# 5 * 2 = 10 pairs. We never correlate a stock with
# itself, since that is always exactly 1.
pairs <- combn(tickers, 2)
n_pairs <- ncol(pairs)
pair_labels <- apply(pairs, 2, function(p) { paste(p[1], p[2], sep = "-") })

# ---- Compute rolling correlation for every pair ----
# One column per pair, one matrix per window length. We fill
# them with a simple loop so each step is easy to read.
roll_cor_short <- xts(matrix(NA, nrow = nrow(returns_xts), ncol = n_pairs),
                      order.by = index(returns_xts))
roll_cor_long <- roll_cor_short
colnames(roll_cor_short) <- pair_labels
colnames(roll_cor_long)  <- pair_labels

# Quick Examination
roll_cor_short
roll_cor_long

# Now fill them all in
for (i in 1:n_pairs) {
  a <- pairs[1, i]
  b <- pairs[2, i]
  roll_cor_short[, i] <- runCor(returns_xts[, a], returns_xts[, b], n = SHORT_WINDOW)
  roll_cor_long[, i]  <- runCor(returns_xts[, a], returns_xts[, b], n = LONG_WINDOW)
}

# The first (window - 1) rows of every column are NA. A
# rolling window needs a full window of data before it can
# produce its first value, so every rolling metric "starts
# late." 

# ---- Plot 1: all pairs, short window ----
# Watch the whole band move together. When the lines bunch up
# high at the same time, the five stocks are moving as one and
# diversification is temporarily weak, which is exactly when
# you would least want it to be.
plot.zoo(as.zoo(roll_cor_short), plot.type = "single",
         col = 1:n_pairs, lty = 1,
         xlab = "", ylab = "Rolling correlation",
         main = paste0(SHORT_WINDOW, "-day rolling correlation, all pairs"))
abline(h = 0, col = "grey60", lty = 2)
legend("bottomleft", legend = pair_labels, col = 1:n_pairs,
       lty = 1, cex = 0.6, bty = "n", ncol = 2)

# TIP: correlations tend to spike toward 1 during market
# stress. The optimizer in B and C used ONE average
# correlation over the whole window; this plot shows how much
# that single number hides.

# ---- Plot 2: one pair, short vs long window ----
# Change FOCUS_INDEX to any pair number (1 through 10) to
# inspect a different relationship.
FOCUS_INDEX <- 1
FOCUS_PAIR <- pair_labels[FOCUS_INDEX]

focus_pair_xts <- merge(roll_cor_short[, FOCUS_INDEX],
                        roll_cor_long[, FOCUS_INDEX])
colnames(focus_pair_xts) <- c("Short", "Long")

plot.zoo(as.zoo(focus_pair_xts), plot.type = "single",
         col = c("steelblue", "darkorange"), lwd = c(1, 2), lty = 1,
         xlab = "", ylab = "Rolling correlation",
         main = paste0(FOCUS_PAIR, ": ", SHORT_WINDOW, "-day vs ", LONG_WINDOW, "-day"))
legend("bottomleft",
       legend = c(paste0(SHORT_WINDOW, "-day"), paste0(LONG_WINDOW, "-day")),
       col = c("steelblue", "darkorange"), lty = 1, lwd = c(1, 2), bty = "n")

# TIP: the blue (short) line jumps around; the orange (long)
# line tells the calmer underlying story. Neither is "right."
# An investor watches the short window for early warnings and
# the long window to avoid overreacting to noise.

# ---- Save for F and the dashboard export ----
saveRDS(roll_cor_short, file.path(loadPth, "roll_cor_short.rds"))
saveRDS(roll_cor_long, file.path(loadPth, "roll_cor_long.rds"))

# End 