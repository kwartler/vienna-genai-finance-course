# ============================================================
# D_compare_portfolios.R
# Portfolio Optimization lesson, Day 2, Data Science Masters
# track (8:00 to 9:00)
#
# Purpose: put the two optimized portfolios side by side,
# along with a naive equal weight benchmark, and compare
# weights, return, risk, and Sharpe ratio in one view.
# Run A, B, and C first: this script only reads their output.
# ============================================================

# ---- Load everything the earlier scripts saved ----
returns_df         <- readRDS("returns_df.rds")
min_var_weights    <- readRDS("min_var_weights.rds")
max_sharpe_weights <- readRDS("max_sharpe_weights.rds")
RISK_FREE_RATE_ANNUAL <- readRDS("risk_free_rate.rds")
# The rate is read back from C's saved file instead of being
# retyped here, so the table below is guaranteed to use the
# same number the optimizer solved with.

tickers <- colnames(returns_df)
n_assets <- length(tickers)

# ---- Guard: are all the files from the same run? ----
# If you swapped to the second ticker basket in A but forgot
# to re-run B and C, the saved weights would refer to the
# WRONG companies. This is exactly the kind of silent
# mismatch an automated pipeline will happily ship and a
# human check catches in two seconds.
if (identical(names(min_var_weights), tickers) == FALSE) {
  stop("Ticker mismatch between A and B output. Re-run A, then B, then C.")
}
if (identical(names(max_sharpe_weights), tickers) == FALSE) {
  stop("Ticker mismatch between A and C output. Re-run A, then C.")
}

# ---- Equal weight benchmark ----
# 1/N: no optimization at all, just an even split of capital.
# It is a famously hard benchmark to beat out of sample,
# which keeps us honest about how much the optimizers are
# really adding.
equal_weights <- rep(1 / n_assets, n_assets)
names(equal_weights) <- tickers

# ---- One function to score any set of weights ----
calcPortfolioStats <- function(weights, returns_df, rf_annual) {
  mean_daily <- colMeans(returns_df)
  cov_daily <- cov(returns_df)
  ret_annual <- as.numeric(t(weights) %*% mean_daily) * 252
  vol_annual <- sqrt(as.numeric(t(weights) %*% cov_daily %*% weights)) * sqrt(252)
  sharpe_annual <- (ret_annual - rf_annual) / vol_annual
  result <- c(Return = ret_annual,
              Volatility = vol_annual,
              Sharpe = sharpe_annual)
  return(result)
}
# TIP: all three portfolios are scored by the SAME function
# on the SAME data. Any difference in the table below comes
# purely from the weights, nothing else. That is what makes
# the comparison fair.

# ---- Build the comparison table ----
stats_equal      <- calcPortfolioStats(equal_weights, returns_df, RISK_FREE_RATE_ANNUAL)
stats_min_var    <- calcPortfolioStats(min_var_weights, returns_df, RISK_FREE_RATE_ANNUAL)
stats_max_sharpe <- calcPortfolioStats(max_sharpe_weights, returns_df, RISK_FREE_RATE_ANNUAL)

summary_table <- rbind(EqualWeight = stats_equal,
                       MinVariance = stats_min_var,
                       MaxSharpe   = stats_max_sharpe)
summary_table <- round(summary_table, 4)

cat("\nAnnualized portfolio comparison:\n")
print(summary_table)

# TIP: read the table one column at a time. MinVariance
# should win the Volatility column, MaxSharpe should win the
# Sharpe column, and EqualWeight usually lands in the middle
# on both. If either optimizer LOSES its own column,
# something upstream is wrong, and noticing that is more
# valuable than the table itself.

# ---- Side by side weights chart ----
weights_matrix <- rbind(EqualWeight = equal_weights,
                        MinVariance = min_var_weights,
                        MaxSharpe   = max_sharpe_weights)

barplot(weights_matrix,
        beside = TRUE,
        col = c("grey70", "steelblue", "darkorange"),
        ylim = c(0, max(weights_matrix) * 1.2),
        main = "Portfolio Weights by Method",
        ylab = "Weight",
        legend.text = rownames(weights_matrix),
        args.legend = list(x = "topright", bty = "n"))

# TIP: the visual story to look for. Equal weight is flat by
# construction. Minimum variance spreads out but tilts toward
# the calmer names. Max Sharpe usually concentrates hard into
# two or three tickers: it is chasing return per unit of risk
# and has no reason to hold anything mediocre.

# ---- Export the results so a person (or an LLM) can read them ----
# jsonlite ships with most R installs; install.packages("jsonlite") if not.
library(jsonlite)

export_list <- list(
  tickers = tickers,
  risk_free_rate_annual = RISK_FREE_RATE_ANNUAL,
  weights = list(
    equal_weight = as.list(round(equal_weights, 4)),
    min_variance = as.list(round(min_var_weights, 4)),
    max_sharpe   = as.list(round(max_sharpe_weights, 4))
  ),
  stats = as.list(as.data.frame(summary_table))
)

write_json(export_list, "portfolio_summary.json", pretty = TRUE, auto_unbox = TRUE)
cat("\nWrote portfolio_summary.json\n")

# WHY EXPORT? R did every calculation here; this JSON is just
# the finished numbers. That is the course principle in action:
# the math lives in R, and an LLM only reads and interprets the
# result, it never recomputes it. The SPA you build later will
# fetch its own live data instead of this file, but the split
# is the same: numbers are computed, the model explains them.
#
# To see the interpretation step, open a chatbot such as
# duck.ai, paste the JSON, and try this prompt:
#
#   "You are a portfolio analyst. Below is JSON with three
#    portfolios (equal weight, minimum variance, maximum
#    Sharpe) built from the same five stocks, plus each one's
#    annualized return, volatility, and Sharpe ratio. In plain
#    English: (1) explain how the three differ in what they
#    optimize for, (2) point out which names each concentrates
#    in or avoids, (3) name two risks a human should check
#    before trusting these weights. Do not recalculate any
#    numbers; interpret only what is given.
#    <paste portfolio_summary.json here>"

# ---- Closing thought ----
# All three of these portfolios are DRAFTS built from two
# years of history. Before any of them touches real capital,
# a human reviews the weights, questions the inputs, and
# makes the call. Knight Capital automated that final human
# step away, and it cost them the firm in 45 minutes.
