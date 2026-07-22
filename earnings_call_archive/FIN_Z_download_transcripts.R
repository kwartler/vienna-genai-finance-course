# ============================================================
# FIN_Z_download_transcripts.R
# INSTRUCTOR ONLY. Not for student use.
#
# Purpose: pre-download S&P 500 earnings call transcripts with
# the instructor API key so students never spend their own
# 250 requests per day limit during class.
#
# Symbols come from a local CSV (sp500_companies.csv), not
# from the API, so the constituent lookup costs zero requests
# and the full daily budget goes to transcripts.
#
# Transcripts are saved exactly as the API returns them (raw
# JSON), one file per symbol/year/quarter, into OUT_DIR. The
# student scripts (Day 2 Finance track) read those files from
# disk instead of calling the API.
#
# Designed to be run repeatedly over several days:
#   - a manifest records every attempt, so re-runs skip work
#     already done (dedupe)
#   - transient failures are retried with backoff
#   - a daily budget stops the run before the API cuts it off
#
# Usage:
#   Sys.setenv(FMP_API_KEY = "your_key_here")   # never hardcode
#   source("FIN_Z_download_transcripts.R")
# Run it again tomorrow. It picks up where it left off.
# ============================================================

# ---- Libraries ----
library(httr)
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

# All paths hang off one archive folder so nothing collides.
ARCHIVE_DIR <- "~/Desktop/vienna-genai-finance-course/earnings_call_archive"

SYMBOLS_CSV <- file.path(ARCHIVE_DIR, "sp500_companies.csv")   # INPUT, read only
OUT_DIR <- file.path(ARCHIVE_DIR, "transcripts_sp500")         # where JSON files land
MANIFEST_FILE <- file.path(ARCHIVE_DIR, "_manifest.csv")       # progress tracking

# NOTE: SYMBOLS_CSV and MANIFEST_FILE must never point at the
# same file. The manifest is rewritten after every call, so
# aiming it at the companies CSV would destroy the symbol list.

# The free tier is 250 requests per day. We stop below that so
# retries stay inside the limit.
DAILY_BUDGET <- 235

# What to collect. Start narrow, widen later: every extra
# quarter multiplies the number of days needed.
TARGET_YEARS <- c(2025, 2024)
TARGET_QUARTERS <- c(1, 2, 3, 4)

# FETCH_MODE controls how many calls each symbol costs.
#   "quarter" = one call per symbol/year/quarter (stable endpoint,
#               works on the free tier, 4 calls per symbol-year)
#   "batch"   = one call per symbol/year returning all quarters
#               (legacy v4 endpoint, 4x cheaper, but may be
#               gated on some plans; try it and see)
FETCH_MODE <- "batch"  # "quarter"

# Retry behaviour for transient failures only.
MAX_ATTEMPTS <- 3
BACKOFF_BASE_SECONDS <- 3
PAUSE_BETWEEN_CALLS <- 0.4

# Limit how many symbols to consider (NA means all of them).
# Useful for a first test run: set to 5, confirm it works,
# then set back to NA.
SYMBOL_LIMIT <- 5

# Download order. The budget spans several days, so the order
# decides what exists first.
#   "weight" = largest index constituents first (AAPL, NVDA,
#              MSFT and so on), which is what students recognize
#   "csv"    = whatever order the CSV happens to be in
SYMBOL_ORDER <- "weight"

# Optional sector filter. NA means every sector. Otherwise
# give a vector, for example c("Technology", "Healthcare").
# Sector names must match the CSV exactly.
SECTOR_FILTER <- NA

# ---- Endpoints ----
URL_TRANSCRIPT_STABLE <- "https://financialmodelingprep.com/stable/earning-call-transcript"
URL_TRANSCRIPT_BATCH <- "https://financialmodelingprep.com/api/v4/batch_earning_call_transcript"

# ============================================================
# SETUP
# ============================================================

api_key <- Sys.getenv("FMP_API_KEY")
if (nchar(api_key) == 0) {
  stop("FMP_API_KEY is not set. Run Sys.setenv(FMP_API_KEY = \"your_key\") first. Do not hardcode the key in this file.")
} else {
  cat("API key found.\n")
}

if (dir.exists(path.expand(OUT_DIR)) == FALSE) {
  dir.create(path.expand(OUT_DIR), recursive = TRUE)
  cat("Created output directory:", OUT_DIR, "\n")
}

# Counts every request made in this run so the budget is honest.
calls_used <- 0

# ============================================================
# HELPERS
# ============================================================

# Read the S&P 500 symbols from the local CSV. Costs no API
# calls. Expects a "Symbol" column; "Sector" and "Weight" are
# used when present.
readSymbolsFromCsv <- function() {
  csv_path <- path.expand(SYMBOLS_CSV)

  if (file.exists(csv_path) == FALSE) {
    stop(paste("Symbols CSV not found at:", csv_path))
  }

  companies <- read.csv(csv_path, stringsAsFactors = FALSE)

  if ("Symbol" %in% names(companies) == FALSE) {
    stop(paste("The CSV has no 'Symbol' column. Columns found:",
               paste(names(companies), collapse = ", ")))
  }

  cat("Read", nrow(companies), "companies from", basename(csv_path), "\n")

  # Optional sector filter.
  if (length(SECTOR_FILTER) > 1 || is.na(SECTOR_FILTER[1]) == FALSE) {
    if ("Sector" %in% names(companies) == TRUE) {
      before <- nrow(companies)
      companies <- companies[companies$Sector %in% SECTOR_FILTER, ]
      cat("Sector filter applied:", before, "->", nrow(companies), "companies\n")
    } else {
      cat("SECTOR_FILTER was set but the CSV has no 'Sector' column. Ignoring it.\n")
    }
  }

  # Order the download queue.
  if (SYMBOL_ORDER == "weight" && "Weight" %in% names(companies) == TRUE) {
    companies <- companies[order(companies$Weight, decreasing = TRUE), ]
    cat("Ordered by index weight, largest first.\n")
  } else {
    cat("Using CSV order.\n")
  }

  syms <- trimws(as.character(companies$Symbol))
  syms <- unique(syms[is.na(syms) == FALSE & nchar(syms) > 0])

  return(syms)
}

# Load the manifest, or start an empty one.
loadManifest <- function() {
  mpath <- path.expand(MANIFEST_FILE)
  if (file.exists(mpath) == TRUE) {
    m <- read.csv(mpath, stringsAsFactors = FALSE)
    return(m)
  } else {
    m <- data.frame(
      symbol = character(0), year = integer(0), quarter = integer(0),
      status = character(0), file = character(0), bytes = integer(0),
      attempts = integer(0), last_try = character(0),
      stringsAsFactors = FALSE
    )
    return(m)
  }
}

# Written after every single call so an interrupted run
# (or a closed laptop) loses nothing.
saveManifest <- function(m) {
  write.csv(m, path.expand(MANIFEST_FILE), row.names = FALSE)
}

# One request with retry. Returns a list describing what
# happened so the caller can decide whether to record a
# permanent result or try again another day.
#
# outcome is one of:
#   "ok"        got data, save it
#   "empty"     valid response, no transcript exists (permanent)
#   "auth"      key rejected, abort the whole run
#   "ratelimit" daily limit hit, stop cleanly for today
#   "error"     transient, worth retrying on a later run
fetchWithRetry <- function(url, query) {
  attempt <- 1
  while (attempt <= MAX_ATTEMPTS) {
    calls_used <<- calls_used + 1
    resp <- tryCatch(
      GET(url, query = query, timeout(45)),
      error = function(e) { return(NULL) }
    )

    if (is.null(resp) == TRUE) {
      # Network level failure (DNS, timeout, dropped connection).
      Sys.sleep(BACKOFF_BASE_SECONDS * attempt)
      attempt <- attempt + 1
      next
    }

    code <- status_code(resp)

    if (code == 200) {
      txt <- content(resp, as = "text", encoding = "UTF-8")
      trimmed <- trimws(txt)
      # An empty array or empty object means the API answered
      # correctly and simply has no transcript for that slot.
      if (nchar(trimmed) == 0 || trimmed == "[]" || trimmed == "{}") {
        return(list(outcome = "empty", text = trimmed, attempts = attempt))
      } else {
        return(list(outcome = "ok", text = txt, attempts = attempt))
      }
    }

    if (code == 401 || code == 403) {
      return(list(outcome = "auth", text = NA, attempts = attempt))
    }

    if (code == 429) {
      return(list(outcome = "ratelimit", text = NA, attempts = attempt))
    }

    if (code >= 500) {
      # Server side problem, worth another try.
      Sys.sleep(BACKOFF_BASE_SECONDS * attempt)
      attempt <- attempt + 1
      next
    }

    # Any other 4xx is a permanent problem with this request.
    return(list(outcome = "error", text = NA, attempts = attempt))
  }
  return(list(outcome = "error", text = NA, attempts = MAX_ATTEMPTS))
}

# ============================================================
# BUILD THE WORK LIST
# ============================================================

manifest <- loadManifest()
symbols <- readSymbolsFromCsv()

if (is.na(SYMBOL_LIMIT) == FALSE) {
  symbols <- head(symbols, SYMBOL_LIMIT)
  cat("SYMBOL_LIMIT is set: only considering", length(symbols), "symbols.\n")
  cat("Symbols this run:", paste(symbols, collapse = ", "), "\n")
}

# The full grid of everything we would eventually like to have.
if (FETCH_MODE == "batch") {
  targets <- expand.grid(symbol = symbols, year = TARGET_YEARS,
                         quarter = NA, stringsAsFactors = FALSE)
} else {
  targets <- expand.grid(symbol = symbols, year = TARGET_YEARS,
                         quarter = TARGET_QUARTERS, stringsAsFactors = FALSE)
}

# expand.grid varies the first column fastest, which would
# interleave symbols. Re-sort so the queue follows the symbol
# order chosen above.
targets$sym_rank <- match(targets$symbol, symbols)
targets <- targets[order(targets$sym_rank, targets$year), ]

# DEDUPE: anything already resolved permanently is dropped.
# "ok" means we have the file. "empty" means the API told us
# no transcript exists, so asking again tomorrow is wasted.
# "error" rows stay in the list and get another chance, up to
# MAX_ATTEMPTS accumulated across runs.
makeKey <- function(sym, yr, qtr) {
  q <- ifelse(is.na(qtr) == TRUE, "B", as.character(qtr))
  return(paste(sym, yr, q, sep = "|"))
}

targets$key <- makeKey(targets$symbol, targets$year, targets$quarter)

if (nrow(manifest) > 0) {
  manifest$key <- makeKey(manifest$symbol, manifest$year, manifest$quarter)
  done_keys <- manifest$key[manifest$status == "ok" | manifest$status == "empty"]
  exhausted_keys <- manifest$key[manifest$status == "error" & manifest$attempts >= MAX_ATTEMPTS]
  skip_keys <- unique(c(done_keys, exhausted_keys))
  targets <- targets[targets$key %in% skip_keys == FALSE, ]
} else {
  manifest$key <- character(0)
}

remaining <- nrow(targets)
cat("\n---- Plan ----\n")
cat("Fetch mode:", FETCH_MODE, "\n")
cat("Remaining targets:", remaining, "\n")
cat("Daily budget:", DAILY_BUDGET, "calls\n")
if (remaining > 0) {
  cat("Estimated days to finish:", ceiling(remaining / DAILY_BUDGET), "\n")
} else {
  cat("Nothing left to download. All targets are resolved.\n")
}
cat("--------------\n\n")

# ============================================================
# DOWNLOAD LOOP
# ============================================================

downloaded <- 0
empties <- 0
errors <- 0
stop_reason <- "budget or work list exhausted"

if (remaining > 0) {
  for (i in 1:remaining) {
    if (calls_used >= DAILY_BUDGET) {
      stop_reason <- "daily budget reached"
      break
    }

    sym <- targets$symbol[i]
    yr <- targets$year[i]
    qtr <- targets$quarter[i]

    if (FETCH_MODE == "batch") {
      url <- paste0(URL_TRANSCRIPT_BATCH, "/", sym)
      qry <- list(year = yr, apikey = api_key)
      fname <- file.path(path.expand(OUT_DIR), paste0(sym, "_", yr, "_all.json"))
    } else {
      url <- URL_TRANSCRIPT_STABLE
      qry <- list(symbol = sym, year = yr, quarter = qtr, apikey = api_key)
      fname <- file.path(path.expand(OUT_DIR), paste0(sym, "_", yr, "Q", qtr, ".json"))
    }

    res <- fetchWithRetry(url, qry)

    if (res$outcome == "auth") {
      stop_reason <- "API key rejected, stopping"
      cat("\nAPI key was rejected. Stopping so no further calls are wasted.\n")
      break
    }

    if (res$outcome == "ratelimit") {
      stop_reason <- "API returned rate limit, stopping for today"
      cat("\nRate limit hit. Stopping cleanly. Run again tomorrow.\n")
      break
    }

    status <- res$outcome
    bytes <- 0
    label <- ifelse(is.na(qtr) == TRUE, paste0(yr), paste0(yr, "Q", qtr))

    if (res$outcome == "ok") {
      # Saved exactly as returned, no reshaping, so students
      # work with the real API response format.
      writeLines(res$text, fname, useBytes = TRUE)
      bytes <- nchar(res$text, type = "bytes")
      downloaded <- downloaded + 1
      cat(sprintf("[%d/%d] %-6s %-7s saved (%s KB)\n", i, remaining, sym, label,
                  format(round(bytes / 1024, 1))))
    } else if (res$outcome == "empty") {
      empties <- empties + 1
      fname <- NA
      cat(sprintf("[%d/%d] %-6s %-7s no transcript\n", i, remaining, sym, label))
    } else {
      errors <- errors + 1
      fname <- NA
      cat(sprintf("[%d/%d] %-6s %-7s ERROR (will retry next run)\n", i, remaining, sym, label))
    }

    # Update the manifest row for this target, then write to
    # disk immediately so progress survives an interruption.
    this_key <- targets$key[i]
    prior_attempts <- 0
    if (this_key %in% manifest$key == TRUE) {
      prior_attempts <- manifest$attempts[manifest$key == this_key][1]
      manifest <- manifest[manifest$key != this_key, ]
    }

    new_row <- data.frame(
      symbol = sym, year = yr, quarter = qtr, status = status,
      file = ifelse(is.na(fname) == TRUE, NA, basename(fname)),
      bytes = bytes, attempts = prior_attempts + res$attempts,
      last_try = as.character(Sys.time()), key = this_key,
      stringsAsFactors = FALSE
    )
    manifest <- rbind(manifest, new_row)
    saveManifest(manifest[, names(manifest) != "key"])

    Sys.sleep(PAUSE_BETWEEN_CALLS)
  }
}

# ============================================================
# SUMMARY
# ============================================================

cat("\n---- Run summary ----\n")
cat("Stopped because:", stop_reason, "\n")
cat("Calls used this run:", calls_used, "of", DAILY_BUDGET, "\n")
cat("Files saved:", downloaded, "\n")
cat("No transcript available:", empties, "\n")
cat("Transient errors (retry next run):", errors, "\n")

full_manifest <- loadManifest()
if (nrow(full_manifest) > 0) {
  total_ok <- sum(full_manifest$status == "ok")
  cat("\nTotal files on disk:", total_ok, "\n")
  cat("Output folder:", path.expand(OUT_DIR), "\n")
} else {
  cat("\nNothing on disk yet.\n")
}
cat("Run this script again tomorrow to continue.\n")
cat("---------------------\n")
