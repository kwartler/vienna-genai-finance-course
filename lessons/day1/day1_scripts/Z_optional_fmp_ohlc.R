# ============================================================
# fmp_ohlc.R
# Day 1 reference: fetch daily OHLC from Financial Modeling Prep
#
# Purpose: prove to yourself that Financial Modeling Prep (FMP)
# returns the same daily OHLC bars as Yahoo Finance, using an
# endpoint we can actually call from a browser tab. Our morning
# R work uses Yahoo (through quantmod) because it is free and
# comfortable from R. Our Repo 1 and Repo 2 SPA builds cannot
# call Yahoo from the browser (CORS blocks it), so the app calls
# FMP instead. Same data, different fetch mechanism.
#
# Run this once with your own key so you have seen the shape of
# the response before you meet it inside a JavaScript file. This
# is the exact endpoint the Repo 1 SPA uses.
# ============================================================

# ---- Libraries ----
# httr:     makes the web request
# jsonlite: parses the JSON body FMP returns
library(httr)
library(jsonlite)

# ---- API key ----
# Sign up (free) at https://site.financialmodelingprep.com/ and
# generate a key from your dashboard. Put the key in ~/.Renviron
# as:
#
#   FMP_API_KEY=your_key_here
#
# Save the file, then restart R (Session menu > Restart R) so
# Sys.getenv sees it. Never paste the key into this script and
# never commit .Renviron.
FMP_API_KEY <- Sys.getenv("FMP_API_KEY")

if (FMP_API_KEY == "") {
  stop(
    "FMP_API_KEY environment variable is not set. Sign up at ",
    "https://site.financialmodelingprep.com/, generate a key, add ",
    "FMP_API_KEY=your_key to ~/.Renviron, and restart R."
  )
}

# ---- Constants (change these, not the code below) ----
TICKER     <- "AAPL"
START_DATE <- "2024-01-01"
END_DATE   <- "2024-12-31"

# TIP: dates are strings in YYYY-MM-DD format. FMP returns the
# full daily history in the window you ask for, so most single
# ticker windows fit in one call. If your call returns fewer
# rows than expected, check the date range and the ticker
# symbol first.

# ---- Rate limits worth knowing ----
# The free plan caps you at a few hundred requests per day (250
# at the time of writing). That is enough for classroom work but
# not for a tight loop over the S&P 500. If you get an error
# mentioning limits, wait and try again, or check your usage on
# the FMP dashboard.

# ---- Build and send the request ----
# The endpoint is /stable/historical-price-eod/full. It returns
# one row per trading day, the same granularity as Yahoo's daily
# bars in A_data_prep.R. The parameters are symbol, from, to,
# and apikey. This is the same URL the Repo 1 SPA builds in
# main.js, so you will recognize it when you read the browser
# code.
FMP_URL <- "https://financialmodelingprep.com/stable/historical-price-eod/full"

response <- GET(
  FMP_URL,
  query = list(
    symbol = TICKER,
    from   = START_DATE,
    to     = END_DATE,
    apikey = FMP_API_KEY
  ),
  timeout(30)
)

# ---- Check the response before parsing ----
# httr's status_code is the HTTP status. 200 is success. 401
# usually means the key is wrong or missing. 429 means you hit
# a rate limit. Anything else, read the message body.
if (status_code(response) != 200) {
  stop(
    "FMP request failed with HTTP status ",
    status_code(response),
    ". Body: ",
    content(response, as = "text", encoding = "UTF-8")
  )
}

raw_text <- content(response, as = "text", encoding = "UTF-8")
parsed   <- fromJSON(raw_text, flatten = TRUE)

# TIP: FMP can return HTTP 200 with an error object in the body
# (for example, a bad key or plan problem), shaped like
# { "Error Message": "..." }. Check for that before you use the
# data.
if (is.null(parsed$`Error Message`) == FALSE) {
  stop("FMP returned an error: ", parsed$`Error Message`)
}

# ---- Pull out the price rows ----
# The stable endpoint returns a bare JSON array of daily bars,
# which fromJSON turns into a data.frame directly. Older FMP
# paths nest the same rows under a "historical" field, so we
# handle both shapes, exactly like the SPA's main.js does.
if (is.data.frame(parsed)) {
  prices <- parsed
} else if (is.null(parsed$historical) == FALSE) {
  prices <- parsed$historical
} else {
  stop("No price data returned for ", TICKER, ". Check the ticker and dates.")
}

if (nrow(prices) == 0) {
  stop("No price data returned for ", TICKER, ". Check the ticker and dates.")
}

# ---- Inspect what came back ----
# Each row has: symbol, date, open, high, low, close, volume
# (and a few extras such as change and vwap). FMP returns rows
# newest first, opposite to how Yahoo returns them through
# quantmod.
cat("Symbol: ", TICKER,        "\n")
cat("Rows:   ", nrow(prices),  "\n\n")

# ---- Clean the response into a usable data frame ----
# Turn date into a proper Date, and make sure the OHLC and
# volume columns are numeric, so downstream code can filter,
# sort, and do math on them.
prices$date   <- as.Date(prices$date)
prices$open   <- as.numeric(prices$open)
prices$high   <- as.numeric(prices$high)
prices$low    <- as.numeric(prices$low)
prices$close  <- as.numeric(prices$close)
prices$volume <- as.numeric(prices$volume)

# Keep just the columns we care about, in a familiar order.
prices <- prices[, c("date", "open", "high", "low", "close", "volume")]

# Reorder oldest first so the frame reads like a normal time
# series. This matches how Yahoo bars look in A_data_prep.R.
prices <- prices[order(prices$date), ]

# ---- Preview ----
# Look at the first and last few rows. If you also ran
# A_data_prep.R on AAPL for the same window, the close column
# here should match the Yahoo adjusted close within a small
# tolerance (data providers adjust differently for some events).
cat("First 5 rows:\n")
print(head(prices, 5))

cat("\nLast 5 rows:\n")
print(tail(prices, 5))

# ---- Save for downstream use (optional) ----
# Uncomment if you want to hand this off to another script, the
# same way A_data_prep.R saves .rds files for B, C, and D to
# read.
# savePth <- '~/Desktop/vienna-genai-finance-course/personalFiles'
# pth <- file.path(savePth, paste0(Sys.Date(),'_fmp_',TICKER, '.rds'))
# saveRDS(prices, pth)

# ---- What to notice ----
# 1. The columns are identical in meaning to Yahoo's OHLCV
#    output. Same market, same day, same numbers within a
#    rounding tolerance.
# 2. The Repo 1 and Repo 2 SPAs use this exact endpoint from
#    JavaScript. When you read the browser code and see the
#    URL, you will recognize it.
# 3. The single script pattern (build URL, GET, check status,
#    parse JSON, cast types, sort) is the same shape you will
#    see in FIN_B (Riskline) and FIN_C (NewsAPI). Different
#    services, same seven step recipe.
