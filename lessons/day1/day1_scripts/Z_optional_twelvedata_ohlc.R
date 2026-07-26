# ============================================================
# twelvedata_ohlc.R
# Day 1 reference: fetch daily OHLC from Twelve Data
#
# Purpose: prove to yourself that Twelve Data returns the same
# daily OHLC bars as Yahoo Finance, using an endpoint we can
# actually call from a browser tab. Our morning R work uses
# Yahoo (through quantmod) because it is free and comfortable
# from R. Our Repo 1 and Repo 2 SPA builds cannot call Yahoo
# from the browser (CORS blocks it), so the app calls Twelve
# Data instead. Same data, different fetch mechanism.
#
# Run this once with your own key so you have seen the shape
# of the response before you meet it inside a JavaScript file.
# ============================================================

# ---- Libraries ----
# httr:     makes the web request
# jsonlite: parses the JSON body Twelve Data returns
library(httr)
library(jsonlite)

# ---- API key ----
# Sign up (free) at https://twelvedata.com/ and generate a key
# from your account dashboard. Put the key in ~/.Renviron as:
#
#   TWELVE_DATA_API=your_key_here
#
# Save the file, then restart R (Session menu > Restart R) so
# Sys.getenv sees it. Never paste the key into this script and
# never commit .Renviron.
TWELVE_DATA_KEY <- Sys.getenv("TWELVE_DATA_API")

if (TWELVE_DATA_KEY == "") {
  stop(
    "TWELVE_DATA_API environment variable is not set. Sign up at ",
    "https://twelvedata.com/, generate a key, add ",
    "TWELVE_DATA_API=your_key to ~/.Renviron, and restart R."
  )
}

# ---- Constants (change these, not the code below) ----
TICKER     <- "AAPL"
START_DATE <- "2024-01-01"
END_DATE   <- "2024-12-31"

# TIP: dates are strings in YYYY-MM-DD format. Twelve Data
# accepts up to 5000 daily bars in one request (roughly 20
# years of stock history), so most single ticker windows fit
# in one call. If your call returns fewer rows than expected,
# check the date range and the ticker symbol first.

# ---- Rate limits worth knowing ----
# The free Basic plan gives you 800 API credits per day and 8
# per minute. One /time_series request costs 1 credit. That is
# enough for classroom work but not for a tight loop over the
# S&P 500. If you get an error mentioning rate limits, wait a
# minute and try again.

# ---- Build and send the request ----
# The endpoint is /time_series. interval=1day gives one row
# per trading day, the same granularity as Yahoo's daily bars
# in A_data_prep.R. format=JSON is the default; we name it so
# a reader of this script does not have to check the docs.
TWELVE_DATA_URL <- "https://api.twelvedata.com/time_series"

response <- GET(
  TWELVE_DATA_URL,
  query = list(
    symbol     = TICKER,
    interval   = "1day",
    start_date = START_DATE,
    end_date   = END_DATE,
    apikey     = TWELVE_DATA_KEY,
    format     = "JSON"
  ),
  timeout(30)
)

# ---- Check the response before parsing ----
# httr's status_code is the HTTP status. 200 is success. 401
# usually means the key is wrong or missing. 429 means you hit
# a rate limit. Anything else, read the message body.
if (status_code(response) != 200) {
  stop(
    "Twelve Data request failed with HTTP status ",
    status_code(response),
    ". Body: ",
    content(response, as = "text", encoding = "UTF-8")
  )
}

raw_text <- content(response, as = "text", encoding = "UTF-8")
parsed   <- fromJSON(raw_text, flatten = TRUE)

# TIP: Twelve Data can return HTTP 200 with an error status in
# the JSON body (for example, an unknown ticker). Check the
# body-level status field too, not just the HTTP code.
if (is.null(parsed$status) == FALSE && parsed$status == "error") {
  stop("Twelve Data returned an error: ", parsed$message)
}

# ---- Inspect what came back ----
# parsed$meta describes the instrument. parsed$values is a
# data.frame with columns: datetime, open, high, low, close,
# volume. Twelve Data returns rows newest first, opposite to
# how Yahoo returns them through quantmod.
cat("Symbol:   ", parsed$meta$symbol,   "\n")
cat("Exchange: ", parsed$meta$exchange, "\n")
cat("Currency: ", parsed$meta$currency, "\n")
cat("Interval: ", parsed$meta$interval, "\n")
cat("Rows:     ", nrow(parsed$values),  "\n\n")

# ---- Clean the response into a usable data frame ----
# The API returns everything as strings inside JSON. Cast the
# OHLC and volume columns to numeric, and turn datetime into
# a proper Date so downstream code can filter and sort on it.
prices <- parsed$values
prices$datetime <- as.Date(prices$datetime)
prices$open     <- as.numeric(prices$open)
prices$high     <- as.numeric(prices$high)
prices$low      <- as.numeric(prices$low)
prices$close    <- as.numeric(prices$close)
prices$volume   <- as.numeric(prices$volume)

# Reorder oldest first so the frame reads like a normal time
# series. This matches how Yahoo bars look in A_data_prep.R.
prices <- prices[order(prices$datetime), ]

# ---- Preview ----
# Look at the first and last few rows. If you also ran
# A_data_prep.R on AAPL for the same window, the close column
# here should match the Yahoo adjusted close within a small
# tolerance (Twelve Data adjusts differently for some events).
cat("First 5 rows:\n")
print(head(prices, 5))

cat("\nLast 5 rows:\n")
print(tail(prices, 5))

# ---- Save for downstream use (optional) ----
# Uncomment if you want to hand this off to another script,
# the same way A_data_prep.R saves .rds files for B, C, and D
# to read.
#
# saveRDS(prices, file = paste0("twelvedata_", TICKER, ".rds"))

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
