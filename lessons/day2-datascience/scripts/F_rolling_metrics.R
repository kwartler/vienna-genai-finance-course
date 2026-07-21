# ============================================================
# F_rolling_metrics.R
# Quantitative Extension lesson, Day 2, Data Science Masters
# track (9:00 to 10:00)
#
# Purpose: compute two rolling portfolio diagnostics for the
# equal-weight basket of the five equities from A_data_prep.R:
#   1. rolling Sharpe ratio (return per unit of risk, over time)
#   2. rolling beta vs SPY (market sensitivity, over time)
# Then export everything (correlation from E + these) to one
# JSON file for a human, or a chatbot, to interpret.
# Run A_data_prep.R and E_rolling_correlation.R first.
# ============================================================

# ---- Libraries ----
# TTR: runSD for the rolling standard deviation in Sharpe
# zoo: rollapply for the rolling beta regression
# jsonlite: writes the export file at the end
library(TTR)
library(zoo)
library(jsonlite)

# ---- Load data from earlier scripts ----
returns_xts       <- readRDS("returns_xts.rds")        # 5 stocks (from A)
benchmark_returns <- readRDS("benchmark_returns.rds")  # SPY (from A)
roll_cor_short    <- readRDS("roll_cor_short.rds")     # from E
roll_cor_long     <- readRDS("roll_cor_long.rds")      # from E

tickers <- colnames(returns_xts)
n_assets <- length(tickers)

# ---- Windows and rate (match E and C so the story lines up) ----
SHORT_WINDOW <- 30
LONG_WINDOW  <- 90
RISK_FREE_RATE_ANNUAL <- 0.035
RISK_FREE_RATE_DAILY <- RISK_FREE_RATE_ANNUAL / 252

# ---- Build the equal-weight portfolio return series ----
# We diagnose ONE portfolio here, not each stock. Equal weight
# keeps it simple: the daily portfolio return is just the row
# average across the five stocks.
port_returns <- xts(rowMeans(returns_xts), order.by = index(returns_xts))
colnames(port_returns) <- "EqualWeight"

# ---- Rolling Sharpe ratio ----
# Sharpe = (mean excess return) / (volatility), computed inside
# a moving window and then annualized. We build it from parts
# so each piece is visible.
excess_port <- port_returns - RISK_FREE_RATE_DAILY

roll_mean <- rollapply(excess_port, width = SHORT_WINDOW, FUN = mean, align = "right", fill = NA)
roll_sd   <- runSD(port_returns, n = SHORT_WINDOW)

roll_sharpe <- (roll_mean / roll_sd) * sqrt(252)
colnames(roll_sharpe) <- "RollingSharpe"
# The sqrt(252) annualizes a daily Sharpe. Multiplying a daily
# ratio by the square root of trading days per year is the
# standard convention.

# TIP: a rolling Sharpe near or below 0 means that, over the
# last SHORT_WINDOW days, the portfolio was not paying you for
# the risk you took. A single all-in Sharpe number (like the
# one in D) would hide those stretches entirely.

# ---- Rolling beta vs SPY ----
# Beta = cov(stock, market) / var(market). We run it inside a
# moving window with a small regression-style function.
paired <- merge(port_returns, benchmark_returns, join = "inner")
colnames(paired) <- c("port", "mkt")

calcBeta <- function(window_data) {
  # window_data is a 2-column chunk: port returns and mkt returns
  covariance <- cov(window_data[, "port"], window_data[, "mkt"])
  market_var <- var(window_data[, "mkt"])
  beta <- covariance / market_var
  return(beta)
}

roll_beta <- rollapply(paired, width = SHORT_WINDOW, FUN = calcBeta,
                       by.column = FALSE, align = "right", fill = NA)
colnames(roll_beta) <- "RollingBeta"

# TIP: beta = 1 means the portfolio moves with the market.
# Above 1 is more volatile than the market, below 1 is calmer.
# Watching beta drift over time tells you if your risk profile
# is quietly changing even when your holdings have not.

# ---- Plot: rolling Sharpe and rolling beta ----
plot.zoo(as.zoo(roll_sharpe), col = "steelblue",
         xlab = "", ylab = paste0(SHORT_WINDOW, "-day rolling Sharpe (annualized)"),
         main = "Equal-weight portfolio: rolling Sharpe ratio")
abline(h = 0, col = "grey60", lty = 2)

plot.zoo(as.zoo(roll_beta), col = "darkorange",
         xlab = "", ylab = paste0(SHORT_WINDOW, "-day rolling beta vs SPY"),
         main = "Equal-weight portfolio: rolling beta")
abline(h = 1, col = "grey60", lty = 2)

# ============================================================
# EXPORT: turn the finished numbers into one JSON file
# ============================================================
# This file is the HUMAN REVIEW SURFACE. R did every
# calculation; the JSON is just the finished numbers, in a
# format a person can read and a language model can explain.
# The course principle holds: the LLM handles language and
# interpretation, never the math. It reads these results, it
# does not recompute them.

# We export the most recent value of each rolling metric plus
# a small tail of history, so the file stays readable.
latest_i <- nrow(roll_sharpe)

export_list <- list(
  tickers = tickers,
  window_days = SHORT_WINDOW,
  risk_free_rate_annual = RISK_FREE_RATE_ANNUAL,
  latest = list(
    rolling_sharpe = round(as.numeric(roll_sharpe[latest_i]), 3),
    rolling_beta_vs_spy = round(as.numeric(roll_beta[latest_i]), 3)
  ),
  correlation_latest = as.list(round(as.numeric(roll_cor_short[nrow(roll_cor_short), ]), 3)),
  correlation_pairs = colnames(roll_cor_short)
)
names(export_list$correlation_latest) <- colnames(roll_cor_short)

write_json(export_list, "rolling_metrics.json", pretty = TRUE, auto_unbox = TRUE)
cat("\nWrote rolling_metrics.json\n")

# HOW WE USE IT IN CLASS (fastest path, no building required):
# open a chatbot such as duck.ai, paste the JSON, and ask for
# a plain-English read. Try this prompt:
#
#   "You are a portfolio risk analyst. Below is JSON with an
#    equal-weight portfolio's latest rolling Sharpe ratio,
#    rolling beta versus the S&P 500 (SPY), and the latest
#    rolling correlations between each pair of its five stocks.
#    In plain English: (1) is this portfolio currently being
#    rewarded for its risk? (2) is it more or less volatile
#    than the market right now? (3) are any pairs so highly
#    correlated that diversification is weak? Do not
#    recalculate anything; interpret only the values given.
#    <paste rolling_metrics.json here>"
#
# Same JSON could instead feed a vibe-coded SPA: a Chart.js
# dashboard of these metrics with an OpenRouter research note.
# We are NOT building that today; the chatbot read is the
# point. Either way, a human looks before anything acts.
#
# This is the monitoring surface Knight Capital never had. In
# 2012 automated logic executed for 45 minutes with no human
# reading a dashboard like this one. The metrics exist so a
# person can see trouble and step in before it compounds.
