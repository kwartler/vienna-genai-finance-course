# ============================================================
# B_min_variance.R
# Portfolio Optimization lesson, Day 2, Data Science Masters
#
# Purpose: solve for the minimum variance portfolio using the
# five equities from A_data_prep.R (AAPL, JPM, JNJ, PG, XOM).
# Run A_data_prep.R first, this script reads its saved output.
# ============================================================

# ---- Libraries ----
# quadprog solves the quadratic program behind this style of
# optimization. Dmat, dvec, Amat, bvec, meq below are its
# required argument names, not names we chose ourselves.
library(quadprog)

# ---- Load path ----
loadPth <- '~/Desktop/vienna-genai-finance-course/portfolio_files'

# ---- Load data from A_data_prep.R ----
returns_df <- readRDS(file.path(loadPth, "returns_df.rds"))
tickers <- colnames(returns_df)
n_assets <- length(tickers)

# ---- Build the covariance matrix ----
# This is the single most important input to the optimizer.
# It captures both how volatile each stock is on its own, and
# how the five stocks move together.
cov_matrix <- cov(returns_df)

# Examine
print(round(cov_matrix, 6))

# Compare 
print(round(cor(returns_df), 2))

# Covariance mixes correlation with the size of each 
# stock's own volatility, so the numbers are harder 
# to read on their own, but it is what the optimizer 
# actually solves with.

# ---- Set up the quadratic program ----
# w (weights): The percentages of your money allocated to each asset.
# w': The transpose of your weights (just a mathematical way to line them up for matrix multiplication).
# Sigma (Sigma / Covariance Matrix): A square table that shows how every asset moves relative to every other asset (who moves together, who moves opposite).
# The Math: Multiplying w' * Sigma * w calculates the total risk (variance) of your portfolio.
Dmat <- cov_matrix
dvec <- rep(0, n_assets)
# We only care about variance here, not
# expected return. Expected return enters in C_max_sharpe.R.

# Constraint 1 (equality): weights sum to 1
# Constraints 2 through n_assets + 1 (inequality): each
# weight is greater than or equal to 0. Those constraints are
# what remove short selling from the solution entirely.
Amat <- cbind(rep(1, n_assets), diag(n_assets))
bvec <- c(1, rep(0, n_assets))
meq <- 1
# meq tells solve.QP that only the first column of Amat is an
# equality constraint. Every column after that is treated as
# greater than or equal to its bvec value.

# ---- Solve ----
qp_result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat,
                       bvec = bvec, meq = meq)

min_var_weights <- qp_result$solution
names(min_var_weights) <- tickers
min_var_weights <- round(min_var_weights, 4)

# ---- Sanity check: never trust optimizer output blindly ----
weights_total <- sum(min_var_weights)
if (abs(weights_total - 1) > 0.0002) {
  cat("WARNING: weights do not sum to 1. Sum is:", weights_total, "\n")
} else {
  cat("Weights sum to 1, as expected.\n")
}

cat("\nMinimum variance portfolio weights:\n")
print(min_var_weights)

# ---- Compare to holding each stock alone ----
# Usually 252 trading days per year
asset_vol_annual <- apply(returns_df, 2, sd) * sqrt(252)
port_variance_daily <- t(min_var_weights) %*% cov_matrix %*% min_var_weights
port_vol_annual <- sqrt(port_variance_daily) * sqrt(252)

cat("\nAnnualized volatility, each stock alone:\n")
print(round(asset_vol_annual, 4))

cat("\nAnnualized volatility, minimum variance portfolio:\n")
print(round(port_vol_annual, 4))

# TIP: the portfolio number above should be lower than every
# single stock's own volatility, as long as the five stocks
# are not perfectly correlated. That gap is the entire point
# of diversification, and it is the reason this portfolio is
# not just all weight on whichever stock looked safest alone.

# ---- Save for D_compare_portfolios.R ----
saveRDS(min_var_weights, file.path(loadPth, "min_var_weights.rds"))

# End