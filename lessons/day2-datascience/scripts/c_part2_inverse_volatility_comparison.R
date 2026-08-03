# ============================================================
# D_part2_inverse_volatility.R
# Portfolio Optimization lesson, Day 2, Data Science Masters
#
# Purpose: weight the same five equities by inverse volatility,
# the heuristic the session 3 SPA reproduces in JavaScript, then
# set it beside the minimum variance weights from B so the
# class can see how close the easy method lands to the true
# optimizer. Run A_data_prep.R and B_min_variance.R first,
# this script reads their saved output.
# ============================================================

# ---- Libraries ----
# No solver here. The inverse volatility weights are plain
# arithmetic, and the minimum variance weights are loaded from
# what B_min_variance.R already saved. jsonlite writes the
# summary the language model reads at the end.
library(jsonlite)

# ---- Load path ----
loadPth <- '~/Desktop/vienna-genai-finance-course/portfolio_files'

# ---- Load data from A_data_prep.R and B_min_variance.R ----
returns_df <- readRDS(file.path(loadPth, "returns_df.rds"))
tickers    <- colnames(returns_df)
n_assets   <- length(tickers)

min_var_weights <- readRDS(file.path(loadPth, "min_var_weights.rds"))
min_var_weights <- min_var_weights[tickers]

# Examine
min_var_weights

# ---- Inverse volatility weights ----
# Weight each stock by 1 over its volatility, then normalize so
# the weights sum to 1. Calmer stocks (lower volatility) get
# more capital, jumpier stocks get less. No covariance matrix
# and no solver: this is the whole reason it ports cleanly to
# the browser in the Hour 3 build.
asset_vol_daily <- apply(returns_df, 2, sd)
raw_weights <- 1 / asset_vol_daily
inv_vol_weights <- raw_weights / sum(raw_weights)
names(inv_vol_weights) <- tickers

# ---- Sanity check: never trust weights blindly ----
weights_total <- sum(inv_vol_weights)
if (abs(weights_total - 1) > 0.0002) {
  cat("WARNING: weights do not sum to 1. Sum is:", weights_total, "\n")
} else {
  cat("Weights sum to 1, as expected.\n")
}

cat("\nInverse volatility portfolio weights:\n")
print(round(inv_vol_weights, 4))

# ---- Why each stock got the weight it did ----
# The calmest stock earns the largest weight. Reading the two
# side by side makes the mechanism obvious before 
# you build it in JavaScript.
asset_vol_annual <- asset_vol_daily * sqrt(252)
vol_vs_weight <- data.frame(
  annual_vol = round(asset_vol_annual, 4),
  inv_vol_weight = round(inv_vol_weights, 4)
)
cat("\nAnnualized volatility and the weight it earns:\n")
print(vol_vs_weight)

# ---- Compare the three weightings ----
# Equal weight (1 / n) is the honest benchmark. Minimum
# variance is the true optimizer, loaded from B. Inverse
# volatility is the heuristic under test.
equal_weights <- rep(1 / n_assets, n_assets)
names(equal_weights) <- tickers

weights_table <- data.frame(
  inverse_vol = round(inv_vol_weights[tickers], 4),
  min_variance = round(min_var_weights[tickers], 4),
  equal_weight = round(equal_weights[tickers], 4)
)
cat("\nPortfolio weights by method:\n")
print(weights_table)

# ---- Annualized risk and return for each weighting ----
# Volatility is computed the same way as B_min_variance.R, from
# the covariance matrix, so the minimum variance number here
# matches the number B printed. Return uses simple returns, so
# it annualizes arithmetically (mean times 252).
cov_matrix <- cov(returns_df)
mean_returns <- colMeans(returns_df)

iv_return <- sum(inv_vol_weights * mean_returns) * 252
iv_vol <- sqrt(t(inv_vol_weights) %*% cov_matrix %*% inv_vol_weights) * sqrt(252)

mv_return <- sum(min_var_weights * mean_returns) * 252
mv_vol <- sqrt(t(min_var_weights) %*% cov_matrix %*% min_var_weights) * sqrt(252)

ew_return <- sum(equal_weights * mean_returns) * 252
ew_vol <- sqrt(t(equal_weights) %*% cov_matrix %*% equal_weights) * sqrt(252)

risk_return_table <- data.frame(
  method = c("inverse_vol", "min_variance", "equal_weight"),
  annual_return = round(c(iv_return, mv_return, ew_return), 4),
  annual_vol = round(as.numeric(c(iv_vol, mv_vol, ew_vol)), 4)
)
cat("\nAnnualized risk and return by method:\n")
print(risk_return_table)

cat("\nInverse volatility carries no covariance matrix and no solver,\n")
cat("yet its volatility should sit close to the minimum variance number.\n")
cat("That closeness is why it is an acceptable stand in for the browser build.\n")

# ---- Side by side weights barplot ----
plot_matrix <- rbind(
  inverse_vol = inv_vol_weights[tickers],
  min_variance = min_var_weights[tickers],
  equal_weight = equal_weights[tickers]
)
barplot(
  plot_matrix,
  beside = TRUE,
  col = c("#9A6B2C", "#14213D", "#B0B0B0"),
  border = NA,
  ylab = "Weight",
  main = "Portfolio weights by method",
  legend.text = rownames(plot_matrix),
  args.legend = list(x = "topright", bty = "n")
)

# ---- Save for downstream use ----
saveRDS(inv_vol_weights, file.path(loadPth, "inv_vol_weights.rds"))

# ---- Export for the interpretation step ----
# R produced every number above. The language model reads this
# file and explains the weighting in plain English. It
# interprets, it does not recalculate.
summary_list <- list(
  method = "inverse_volatility",
  note = paste(
    "Inverse volatility weights each stock by 1 over its volatility, then",
    "normalizes. It is the accepted stand in for minimum variance in the",
    "browser build because it needs no covariance matrix and no solver.",
    "Minimum variance is included here, loaded from B, for comparison."
  ),
  tickers = tickers,
  inverse_volatility = list(
    weights = as.list(round(inv_vol_weights[tickers], 6)),
    annual_return = round(iv_return, 6),
    annual_volatility = round(as.numeric(iv_vol), 6)
  ),
  minimum_variance = list(
    weights = as.list(round(min_var_weights[tickers], 6)),
    annual_return = round(mv_return, 6),
    annual_volatility = round(as.numeric(mv_vol), 6)
  ),
  equal_weight = list(
    weights = as.list(round(equal_weights[tickers], 6)),
    annual_return = round(ew_return, 6),
    annual_volatility = round(as.numeric(ew_vol), 6)
  )
)

write_json(summary_list, file.path(loadPth, "inverse_vol_summary.json"),
           pretty = TRUE, auto_unbox = TRUE)
cat("\nWrote inverse_vol_summary.json for the interpretation step.\n")

# End
