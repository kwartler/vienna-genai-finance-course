# ============================================================
# FIN_ZZ_batch_transcripts.R
# REFERENCE ONLY. We do not cover this in class.
#
# Purpose: show how to pull ALL available earnings call
# transcripts for one company in a single year, using one
# API call.
#
# Why this exists: the transcripts we use in class were
# pre-downloaded for you, so you never have to spend your own
# API quota. This script is here for after the course, when
# you want to pull your own data.
#
# Quota note: the free tier allows 250 requests per day. This
# script uses ONE request and can return up to four
# transcripts (Q1 through Q4), which makes it the cheaper of
# the two approaches. Compare with FIN_ZZZ_single_transcript.R,
# which spends one request per quarter.
#
# Caution: this is a legacy (v4) endpoint. It works on most
# plans but is occasionally restricted. If you get an empty
# result, use the single transcript script instead.
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

# ---- What to fetch (change these two lines) ----
SYMBOL <- "AAPL"
YEAR <- 2025

# ---- Build the request ----
base_url <- "https://financialmodelingprep.com/api/v4/batch_earning_call_transcript"
url <- paste0(base_url, "/", SYMBOL)

cat("Requesting all", YEAR, "transcripts for", SYMBOL, "in one call.\n")

response <- GET(url, query = list(year = YEAR, apikey = api_key), timeout(45))

# ---- Check the response before trusting it ----
if (status_code(response) != 200) {
  stop(paste("Request failed with HTTP status", status_code(response),
             "- check your key, the symbol, and your daily quota."))
} else {
  cat("Request succeeded.\n")
}

raw_text <- content(response, as = "text", encoding = "UTF-8")

if (trimws(raw_text) == "[]" || nchar(trimws(raw_text)) == 0) {
  stop("The API returned nothing. Either no transcripts exist for that year, or this endpoint is not available on your plan. Try FIN_ZZZ_single_transcript.R.")
} else {
  cat("Data received.\n")
}

# ---- Look at what came back ----
transcripts <- fromJSON(raw_text)

cat("\nTranscripts returned:", nrow(transcripts), "\n")
cat("Available fields:", paste(names(transcripts), collapse = ", "), "\n\n")

# One row per transcript. Print a short summary of each.
for (i in 1:nrow(transcripts)) {
  cat("  Q", transcripts$quarter[i],
      "  ", transcripts$date[i],
      "  ", format(nchar(transcripts$content[i]), big.mark = ","), "characters\n", sep = "")
}

# TIP: the useful column is `content`, the full text of the
# call. Everything else (symbol, quarter, year, date) is
# metadata you can use to organize the files.

# ---- Preview the start of the first transcript ----
cat("\n---- First 600 characters of Q", transcripts$quarter[1], " ----\n", sep = "")
cat(substr(transcripts$content[1], 1, 600), "\n")
cat("---- end of preview ----\n")

# ---- Save each transcript to disk ----
# Saved in the same raw JSON format and naming convention the
# class files use, so these drop straight in alongside them.
OUT_DIR <- "transcripts_sp500"

if (dir.exists(OUT_DIR) == FALSE) {
  dir.create(OUT_DIR, recursive = TRUE)
}

for (i in 1:nrow(transcripts)) {
  one <- transcripts[i, ]
  fname <- file.path(OUT_DIR, paste0(SYMBOL, "_", YEAR, "Q", one$quarter, ".json"))
  writeLines(toJSON(one, auto_unbox = TRUE, pretty = TRUE), fname, useBytes = TRUE)
  cat("Saved:", fname, "\n")
}

cat("\nDone. One API request spent.\n")
