# ============================================================
# C_max_sharpe.R
# Portfolio Optimization lesson, Day 2, Data Science Masters
# track (8:00 to 9:00)
#
# Purpose: solve for the maximum Sharpe ratio (tangency)
# portfolio for the five equities from A_data_prep.R, using
# the PortfolioAnalytics wrapper. Run A_data_prep.R first.
# A by-hand quadprog version of the same problem is included
# at the bottom, commented out, for the curious student.
# ============================================================

# ---- Libraries ----
# PortfolioAnalytics is a high level wrapper: you DESCRIBE
# the portfolio (constraints plus objectives) and it hands
# the math to a solver behind the scenes. ROI is the bridge
# layer, and ROI.plugin.quadprog is the actual solver, the
# same quadprog engine B_min_variance.R called directly.
# First time only:
# install.packages(c("PortfolioAnalytics", "ROI", "ROI.plugin.quadprog"))
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# ---- Load data from A_data_prep.R ----
returns_xts <- readRDS("returns_xts.rds")
tickers <- colnames(returns_xts)
n_assets <- length(tickers)

# ---- Risk free rate (input: change it here, nowhere else) ----
RISK_FREE_RATE_ANNUAL <- 0.035
# 3.5% is a reasonable classroom value, close to long run
# historical short term rates. In practice the standard proxy
# is the 3-month US Treasury bill yield: look up the current
# value on treasury.gov (Daily Treasury Bill Rates) or FRED
# (series DTB3), then enter it here as a decimal.
RISK_FREE_RATE_DAILY <- RISK_FREE_RATE_ANNUAL / 252
# Dividing by 252 trading days ignores compounding. That is a
# common classroom simplification, fine for comparing
# portfolios against each other.

# ---- Excess returns ----
# PortfolioAnalytics' max Sharpe shortcut assumes a risk free
# rate of ZERO. Subtracting the daily risk free rate from
# every return first bakes our 3.5% into the problem: the max
# Sharpe portfolio on excess returns IS the max Sharpe
# portfolio at our chosen rate, because subtracting a
# constant changes no asset's volatility.
excess_xts <- returns_xts - RISK_FREE_RATE_DAILY

if (any(colMeans(excess_xts) > 0) == TRUE) {
  cat("At least one asset beats the risk free rate, continuing.\n")
} else {
  stop("No asset beats the risk free rate over this window; a max Sharpe portfolio is not meaningful here.")
}

# ---- Describe the portfolio ----
# Notice we never touch a matrix here. We state WHAT we want
# and the wrapper works out HOW.
port_spec <- portfolio.spec(assets = tickers)

port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
# Weights must sum to 1: all capital is deployed.

port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
# No short selling: every weight is greater than or equal
# to 0. Same rule B enforced with its diagonal matrix.

port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
# With BOTH a return objective and a risk objective in place,
# the maxSR flag below tells the optimizer to trade them off
# as a Sharpe ratio rather than optimizing either one alone.

# ---- Solve ----
opt_result <- optimize.portfolio(R = excess_xts,
                                 portfolio = port_spec,
                                 optimize_method = "ROI",
                                 maxSR = TRUE)

max_sharpe_weights <- extractWeights(opt_result)
max_sharpe_weights <- round(max_sharpe_weights, 4)

# ---- Sanity checks: never trust optimizer output blindly ----
weights_total <- sum(max_sharpe_weights)
if (abs(weights_total - 1) > 0.0001) {
  cat("WARNING: weights do not sum to 1. Sum is:", weights_total, "\n")
} else {
  cat("Weights sum to 1, as expected.\n")
}

if (any(max_sharpe_weights < 0) == TRUE) {
  cat("WARNING: a weight solved slightly negative, check solver tolerance.\n")
} else {
  cat("All weights are non-negative, as expected.\n")
}

cat("\nMaximum Sharpe ratio portfolio weights:\n")
print(max_sharpe_weights)

# ---- Portfolio level statistics (on the ORIGINAL returns) ----
mean_returns_daily <- colMeans(returns_xts)
cov_matrix <- cov(returns_xts)

port_return_annual <- as.numeric(t(max_sharpe_weights) %*% mean_returns_daily) * 252
port_variance_daily <- as.numeric(t(max_sharpe_weights) %*% cov_matrix %*% max_sharpe_weights)
port_vol_annual <- sqrt(port_variance_daily) * sqrt(252)
port_sharpe_annual <- (port_return_annual - RISK_FREE_RATE_ANNUAL) / port_vol_annual

cat("\nAnnualized expected return:", round(port_return_annual, 4), "\n")
cat("Annualized volatility:", round(port_vol_annual, 4), "\n")
cat("Annualized Sharpe ratio:", round(port_sharpe_annual, 4), "\n")

# TIP: this portfolio chases return per unit of risk, not low
# risk on its own, so do not be surprised if the weights land
# heavily on two or three tickers while others sit at or near
# zero. Compare that concentration to the minimum variance
# weights from B; the difference in behavior is the whole
# point of this lesson.

# ---- Save for D_compare_portfolios.R ----
saveRDS(max_sharpe_weights, "max_sharpe_weights.rds")
saveRDS(RISK_FREE_RATE_ANNUAL, "risk_free_rate.rds")
# D reads the rate back from this file, so the comparison
# table is guaranteed to use the same number this solver
# solved with.

# ============================================================
# IN DEPTH OPTION (curious students only): the same problem,
# solved by hand with quadprog, the package B used directly.
#
# Why the extra work? The Sharpe ratio is a RATIO of two
# functions of the weights, and solve.QP can only minimize a
# single quadratic function. The classic trick: solve for
# UNNORMALIZED weights y that minimize y' Sigma y, subject to
# y' excess_returns = 1 and y >= 0. Then rescale y so it sums
# to 1. The rescaled vector is the max Sharpe portfolio,
# found through variance minimization instead of ratio
# maximization.
#
# Uncomment the block below and run it. The weights should
# match max_sharpe_weights above to rounding. Same math, two
# levels of abstraction.
#
# library(quadprog)
# excess_means <- colMeans(returns_xts) - RISK_FREE_RATE_DAILY
# Dmat <- cov_matrix
# dvec <- rep(0, n_assets)
# Amat <- cbind(excess_means, diag(n_assets))
# bvec <- c(1, rep(0, n_assets))
# meq  <- 1
# # meq = 1 marks only the FIRST column of Amat as an equality
# # constraint (y' excess_means = 1). The remaining columns are
# # inequalities (each y >= 0), which is what removes short
# # selling from the solution.
# qp_result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat,
#                       bvec = bvec, meq = meq)
# y_unnormalized <- qp_result$solution
# by_hand_weights <- y_unnormalized / sum(y_unnormalized)
# names(by_hand_weights) <- tickers
# print(round(by_hand_weights, 4))
# ============================================================
