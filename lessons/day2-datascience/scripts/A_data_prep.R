# ============================================================
# A_data_prep.R
# Portfolio Optimization lesson, Day 2, Data Science Masters
# track (8:00 to 9:00)
#
# Purpose: fetch price history for 5 equities and build the
# returns data the rest of the lesson reads in:
#   B_min_variance.R        reads returns_df.rds
#   C_max_sharpe.R          reads returns_xts.rds
#   D_compare_portfolios.R  reads returns_df.rds
# Run this script first, every time.
# ============================================================

# ---- Libraries ----
# quantmod:             pulls price data from Yahoo Finance
# PerformanceAnalytics: converts prices to returns cleanly
library(quantmod)
library(PerformanceAnalytics)

# ---- Constants (change these, not the code below) ----
TICKERS <- c("AAPL", "JPM", "JNJ", "PG", "XOM")
# Tech, financials, healthcare, consumer staples, energy:
# five different sectors so the optimizer has real
# diversification benefit to work with later.

# Want to experiment? Comment out the line above, uncomment
# the line below, and re-run this script plus B, C, and D.
# Same five sectors, different companies. Watch how much the
# optimized weights change: that sensitivity to inputs is
# itself a lesson.
# TICKERS <- c("MSFT", "V", "PFE", "KO", "CVX")

BENCHMARK <- "SPY"
# SPY is an ETF that tracks the S&P 500, so it stands in for
# "the market" as a whole. F_rolling_metrics.R needs it to
# compute each stock's beta (how much a stock moves relative
# to the market). It is NOT part of the 5-stock basket that
# B, C, and D optimize over; it is only a reference series.

LOOKBACK_YEARS <- 2
END_DATE   <- Sys.Date()
START_DATE <- END_DATE - (365 * LOOKBACK_YEARS)

# ---- Fetch prices ----
# getSymbols pulls one xts object per ticker into your
# environment (an object literally named AAPL, JPM, etc).
getSymbols(c(TICKERS, BENCHMARK), src = "yahoo", from = START_DATE, to = END_DATE)

# TIP: run `AAPL` alone in the console right now. Look at the
# six columns. You only need Adjusted for this lesson, the
# rest (Open/High/Low/Close/Volume) mattered more for the
# technical indicator work from Day 1.

# ---- Extract adjusted close, align into one object ----
# The 5-stock basket (for B, C, D) and the benchmark (for F)
# are kept in separate objects on purpose, so the benchmark
# never accidentally lands in the optimization.
adj_close_list <- lapply(TICKERS, function(ticker) {
  Ad(get(ticker))
})
prices_xts <- do.call(merge, adj_close_list)
colnames(prices_xts) <- TICKERS

benchmark_prices <- Ad(get(BENCHMARK))
colnames(benchmark_prices) <- BENCHMARK

# ---- Check for missing data ----
if (anyNA(prices_xts) == TRUE) {
  cat("Missing values found, filling forward.\n")
  prices_xts <- na.locf(prices_xts)
} else {
  cat("No missing values, good to go.\n")
}

# TIP: na.locf carries the last known price forward. It is a
# reasonable fix for a holiday or a small data gap, but always
# check *how many* NAs you had. A ticker missing three weeks
# of data is a different problem than one missing a single day.

# ---- Compute returns ----
# We use simple (discrete) returns throughout this lesson:
# the portfolio's return equals the weighted sum of asset
# returns, which is exactly what the optimizers in B and C
# assume.
returns_xts <- Return.calculate(prices_xts, method = "discrete")
# Option for the curious: method = "log". Log returns add
# cleanly ACROSS TIME (handy for multi-period math) but do
# not sum cleanly ACROSS ASSETS, so simple returns are the
# better fit for portfolio weights.

returns_xts <- returns_xts[-1, ]
# The first row is always NA (no prior day to compare
# against), so we drop it here rather than carrying it
# into B and C.

# Same treatment for the benchmark, so its dates line up with
# the 5-stock returns when F merges them.
benchmark_returns <- Return.calculate(benchmark_prices, method = "discrete")
benchmark_returns <- benchmark_returns[-1, ]

returns_df <- as.data.frame(returns_xts)
# Row names are still the trading dates. Keep them; you will
# want them if you plot a return series later.

# ---- Quick look before moving on ----
head(returns_df)
summary(returns_df)

cat("\nCorrelation matrix (preview only, covered fully later):\n")
print(round(cor(returns_df), 2))

# TIP: this correlation matrix is the number the optimizer
# actually cares about. Two stocks with a low or negative
# correlation do more for reducing portfolio risk than two
# more familiar names that tend to move together.

# ---- Save for downstream scripts ----
saveRDS(returns_df, "returns_df.rds")             # read by B and D
saveRDS(returns_xts, "returns_xts.rds")           # read by C and E (xts form)
saveRDS(prices_xts, "prices_xts.rds")             # kept for reference and plotting
saveRDS(benchmark_returns, "benchmark_returns.rds") # read by F for rolling beta
