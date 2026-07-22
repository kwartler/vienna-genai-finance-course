# ============================================================
# FIN_ZZZ_single_transcript.R
# REFERENCE ONLY. We do not cover this in class.
#
# Purpose: the simplest possible version. Fetch ONE earnings
# call transcript for one company, one year, one quarter.
#
# Why this exists: the transcripts we use in class were
# pre-downloaded for you, so you never have to spend your own
# API quota. This script is here for after the course, when
# you want to pull your own data.
#
# Quota note: the free tier allows 250 requests per day. This
# script uses ONE request and returns ONE transcript. If you
# want a full year, FIN_ZZ_batch_transcripts.R gets all four
# quarters for the same single request.
#
# This uses the current (stable) endpoint, so it is the most
# reliable of the two.
# ============================================================

# ---- Libraries ----
library(httr)
library(jsonlite)

# ---- Your API key ----
# Set this in the console before running, so your key never
# gets saved inside a file you might share or commit:
#   Sys.setenv(FMP_API_KEY = "your_key_here")
api_key <- Sys.getenv("FMP_API_KEY")

if (nchar(api_key) == 0) {
  stop("No API key found. Run Sys.setenv(FMP_API_KEY = \"your_key_here\") first.")
} else {
  cat("API key found.\n")
}

# ---- What to fetch (change these three lines) ----
SYMBOL <- "AAPL"
YEAR <- 2025
QUARTER <- 1

# ---- Build the request ----
url <- "https://financialmodelingprep.com/stable/earning-call-transcript"

cat("Requesting", SYMBOL, YEAR, "Q", QUARTER, "\n")

response <- GET(url,
                query = list(symbol = SYMBOL, year = YEAR,
                             quarter = QUARTER, apikey = api_key),
                timeout(45))

# ---- Check the response before trusting it ----
if (status_code(response) != 200) {
  stop(paste("Request failed with HTTP status", status_code(response),
             "- check your key, the symbol, and your daily quota."))
} else {
  cat("Request succeeded.\n")
}

raw_text <- content(response, as = "text", encoding = "UTF-8")

if (trimws(raw_text) == "[]" || nchar(trimws(raw_text)) == 0) {
  stop("No transcript exists for that symbol, year, and quarter. Try a different quarter.")
} else {
  cat("Data received.\n")
}

# ---- Look at what came back ----
# The API returns an array with one element, so we take the
# first row.
transcript <- fromJSON(raw_text)
one <- transcript[1, ]

cat("\nAvailable fields:", paste(names(transcript), collapse = ", "), "\n")
cat("Symbol: ", one$symbol, "\n", sep = "")
cat("Period: ", one$year, " Q", one$quarter, "\n", sep = "")
cat("Date:   ", one$date, "\n", sep = "")
cat("Length: ", format(nchar(one$content), big.mark = ","), " characters\n", sep = "")

# TIP: `content` holds the full text of the call. That single
# field is what you would pass to an LLM for sentiment,
# entity extraction, or summarization. Notice how long it is:
# this is exactly why context length and chunking matter.

# ---- Preview the opening ----
cat("\n---- First 600 characters ----\n")
cat(substr(one$content, 1, 600), "\n")
cat("---- end of preview ----\n")

# ---- Save it ----
# Same raw JSON format and naming convention as the class
# files, so this drops straight in alongside them.
OUT_DIR <- "transcripts_sp500"

if (dir.exists(OUT_DIR) == FALSE) {
  dir.create(OUT_DIR, recursive = TRUE)
}

fname <- file.path(OUT_DIR, paste0(SYMBOL, "_", YEAR, "Q", QUARTER, ".json"))
writeLines(raw_text, fname, useBytes = TRUE)

cat("\nSaved:", fname, "\n")
cat("Done. One API request spent.\n")
