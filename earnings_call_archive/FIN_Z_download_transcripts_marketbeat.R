# ============================================================
# FIN_Z_download_transcripts_marketbeat.R
# INSTRUCTOR ONLY. Not for student use.
#
# Purpose: pre-download S&P 500 earnings call transcripts by
# scraping MarketBeat.com. This is the primary transcript
# pipeline for the course: FMP's free tier does not include the
# earnings-call-transcript endpoint (it's a premium add-on), so
# FIN_Z_download_transcripts.R cannot actually pull transcripts
# on a free key. This script needs no API key and no daily
# quota, but it scrapes a live site, so it is deliberately
# rate-limited and capped per run to stay polite.
#
# Symbols come from sp500_companies.csv, same as the FMP script.
# For each symbol we read its MarketBeat earnings tab to find
# the last N_QUARTERS report pages, then scrape the
# speaker-by-speaker transcript out of each report page.
#
# Two manifests keep this resumable across many short runs:
#   _manifest_marketbeat_listings.csv    symbol -> report URLs/dates
#                                         (fetched once per symbol)
#   _manifest_marketbeat_transcripts.csv one row per symbol/report
#                                         (the actual download)
#
# Usage:
#   source("FIN_Z_download_transcripts_marketbeat.R")
# Run it again to continue where it left off.
# ============================================================

# ---- Libraries ----
library(rvest)
library(httr)

# ============================================================
# CONFIGURATION
# ============================================================

ARCHIVE_DIR <- "~/Desktop/vienna-genai-finance-course/earnings_call_archive"

SYMBOLS_CSV <- file.path(ARCHIVE_DIR, "sp500_companies.csv")       # INPUT, read only
OUT_DIR <- file.path(ARCHIVE_DIR, "transcripts_sp500_marketbeat")  # where CSV files land
LISTINGS_MANIFEST_FILE <- file.path(ARCHIVE_DIR, "_manifest_marketbeat_listings.csv")
TRANSCRIPTS_MANIFEST_FILE <- file.path(ARCHIVE_DIR, "_manifest_marketbeat_transcripts.csv")

# How many past quarters to pull per symbol.
N_QUARTERS <- 8

# MarketBeat publishes no request quota, so this is a self-imposed
# politeness cap rather than a hard limit. Roughly 500 symbols x
# (1 listing page + up to 8 report pages) is ~4500 requests total,
# so a full run of the index takes several sessions at this cap.
RUN_REQUEST_LIMIT <- 400

# Retry behavior for transient failures only.
MAX_ATTEMPTS <- 3
BACKOFF_BASE_SECONDS <- 4
PAUSE_MIN_SECONDS <- 1.2
PAUSE_MAX_SECONDS <- 2.5

# Limit how many symbols to consider (NA means all of them).
# Start small, confirm it works, then set to NA for the full run.
SYMBOL_LIMIT <- NA

# "weight" = largest index constituents first, "csv" = CSV order.
SYMBOL_ORDER <- "weight"

USER_AGENT <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"

# ============================================================
# SETUP
# ============================================================

if (dir.exists(path.expand(OUT_DIR)) == FALSE) {
  dir.create(path.expand(OUT_DIR), recursive = TRUE)
  cat("Created output directory:", OUT_DIR, "\n")
}

requests_used <- 0

# ============================================================
# HTTP HELPERS
# ============================================================

politePause <- function() {
  Sys.sleep(runif(1, PAUSE_MIN_SECONDS, PAUSE_MAX_SECONDS))
}

# One page fetch. Returns outcome + parsed HTML (or NULL).
# outcome is one of: "ok", "notfound", "ratelimit", "error"
fetchHtml <- function(url) {
  requests_used <<- requests_used + 1
  resp <- tryCatch(
    httr::GET(url, httr::user_agent(USER_AGENT), httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp)) return(list(outcome = "error", page = NULL))

  code <- httr::status_code(resp)
  if (code == 200) {
    page <- httr::content(resp, as = "text", encoding = "UTF-8") %>% read_html()
    return(list(outcome = "ok", page = page))
  }
  if (code == 404) return(list(outcome = "notfound", page = NULL))
  if (code == 429) return(list(outcome = "ratelimit", page = NULL))
  return(list(outcome = "error", page = NULL))
}

# Fetch with retry + backoff on transient errors AND on pages that load
# (HTTP 200) but are missing the content we expect. Testing showed
# MarketBeat occasionally serves a 200 with an incomplete render, no
# transcript bubbles, no listing links, that resolves cleanly on a
# retry a few seconds later. Without this, a single bad render gets
# permanently misclassified as "no transcript exists" for a quarter
# that actually has one. contentCheck(page) returns TRUE when the page
# has what we need.
fetchWithRetry <- function(url, contentCheck = function(page) TRUE) {
  attempt <- 1
  while (attempt <= MAX_ATTEMPTS) {
    politePause()
    res <- fetchHtml(url)
    if (res$outcome == "ok" && contentCheck(res$page) == TRUE) {
      return(list(outcome = "ok", page = res$page, attempts = attempt))
    }
    if (res$outcome %in% c("notfound", "ratelimit")) {
      return(list(outcome = res$outcome, page = NULL, attempts = attempt))
    }
    Sys.sleep(BACKOFF_BASE_SECONDS * attempt)
    attempt <- attempt + 1
  }
  return(list(outcome = "empty", page = NULL, attempts = MAX_ATTEMPTS))
}

# ============================================================
# SCRAPE HELPERS
# ============================================================

parse_bubble <- function(bubble) {
  # .secondary-title is nested inside the bold speaker element, so its
  # text is included when the bold element's text is pulled directly.
  # Strip it out so `speaker` isn't "NameTitle" concatenated together.
  speaker_el <- bubble %>% html_element(css = '.transcript-line-speaker .font-weight-bold')
  title <- speaker_el %>% html_element(css = '.secondary-title') %>% html_text(trim = TRUE)
  speaker_full <- speaker_el %>% html_text(trim = TRUE)
  speaker <- trimws(sub(title, "", speaker_full, fixed = TRUE))

  paragraphs <- bubble %>% html_elements(css = '.pb-2') %>% html_text(trim = TRUE)
  msg <- paste(paragraphs, collapse = '\n\n')

  data.frame(speaker = speaker, title = title, msg = msg, stringsAsFactors = FALSE)
}

# The earnings tab lists links to individual report pages
# (/earnings/reports/YYYY-M-D-company-stock/), one per quarter, going
# back several years. The exchange segment in this URL doesn't matter,
# MarketBeat redirects to the ticker's real exchange automatically.
getRecentReportUrls <- function(ticker, n_quarters = N_QUARTERS) {
  listing_url <- paste0("https://www.marketbeat.com/stocks/NASDAQ/", ticker, "/earnings/")
  hasLinks <- function(page) {
    length(page %>% html_elements("a[href*='/earnings/reports/']")) > 0
  }
  res <- fetchWithRetry(listing_url, hasLinks)
  if (res$outcome != "ok") {
    return(list(outcome = res$outcome, dates = as.Date(character(0)), urls = character(0)))
  }

  links <- res$page %>% html_elements("a[href*='/earnings/reports/']") %>% html_attr("href")
  links <- unique(sub("#.*$", "", links))
  links <- links[grepl("^/earnings/reports/", links)]
  if (length(links) == 0) {
    return(list(outcome = "empty", dates = as.Date(character(0)), urls = character(0)))
  }

  report_dates <- as.Date(sub("^/earnings/reports/(\\d{4}-\\d{1,2}-\\d{1,2})-.*", "\\1", links),
                           format = "%Y-%m-%d")
  ord <- order(report_dates, decreasing = TRUE)
  links <- links[ord]
  report_dates <- report_dates[ord]

  # Skip scheduled/future calls, they have no transcript yet.
  past <- !is.na(report_dates) & report_dates <= Sys.Date()
  links <- links[past]
  report_dates <- report_dates[past]

  links <- head(links, n_quarters)
  report_dates <- head(report_dates, n_quarters)

  list(outcome = "ok", dates = report_dates, urls = paste0("https://www.marketbeat.com", links))
}

getTranscriptFromUrl <- function(url) {
  hasBubbles <- function(page) {
    length(page %>% html_elements(css = ".transcript-line-left, .transcript-line-right")) > 0
  }

  res <- fetchWithRetry(url, hasBubbles)

  # Dual-class tickers (e.g. GOOG/GOOGL) get a disambiguated "-1" slug on
  # one class's earnings tab, but MarketBeat only attaches the actual
  # transcript to the other class's un-suffixed page. When a "-N" page
  # comes back with no bubbles, try the de-suffixed URL once before
  # concluding there's genuinely no transcript.
  if (res$outcome != "ok" && grepl("-[0-9]+/$", url)) {
    fallback_url <- sub("-[0-9]+/$", "/", url)
    res <- fetchWithRetry(fallback_url, hasBubbles)
    if (res$outcome == "ok") url <- fallback_url
  }

  if (res$outcome != "ok") return(list(outcome = res$outcome, transcript = NULL, url = url))

  speech_bubbles <- res$page %>% html_elements(css = ".transcript-line-left, .transcript-line-right")
  results_list <- lapply(speech_bubbles, parse_bubble)
  df <- do.call(rbind, results_list)
  list(outcome = "ok", transcript = df, url = url)
}

# ============================================================
# SYMBOL LIST
# ============================================================

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

# ============================================================
# MANIFESTS
# ============================================================

loadListingsManifest <- function() {
  mpath <- path.expand(LISTINGS_MANIFEST_FILE)
  if (file.exists(mpath) == TRUE) {
    return(read.csv(mpath, stringsAsFactors = FALSE))
  }
  data.frame(symbol = character(0), status = character(0),
             n_reports_found = integer(0), attempts = integer(0),
             last_try = character(0), stringsAsFactors = FALSE)
}

saveListingsManifest <- function(m) {
  write.csv(m, path.expand(LISTINGS_MANIFEST_FILE), row.names = FALSE)
}

loadTranscriptsManifest <- function() {
  mpath <- path.expand(TRANSCRIPTS_MANIFEST_FILE)
  if (file.exists(mpath) == TRUE) {
    return(read.csv(mpath, stringsAsFactors = FALSE))
  }
  data.frame(symbol = character(0), report_date = character(0), url = character(0),
             status = character(0), file = character(0), n_speech_turns = integer(0),
             attempts = integer(0), last_try = character(0), stringsAsFactors = FALSE)
}

saveTranscriptsManifest <- function(m) {
  write.csv(m, path.expand(TRANSCRIPTS_MANIFEST_FILE), row.names = FALSE)
}

# ============================================================
# PHASE 1: discover report URLs for each symbol (once per symbol)
# ============================================================

symbols <- readSymbolsFromCsv()
if (is.na(SYMBOL_LIMIT) == FALSE) {
  symbols <- head(symbols, SYMBOL_LIMIT)
  cat("SYMBOL_LIMIT is set: only considering", length(symbols), "symbols.\n")
  cat("Symbols this run:", paste(symbols, collapse = ", "), "\n")
}

listings_manifest <- loadListingsManifest()
resolved_symbols <- listings_manifest$symbol[listings_manifest$status %in% c("ok", "empty")]
symbols_needing_listing <- setdiff(symbols, resolved_symbols)

cat("\n---- Phase 1: report listings ----\n")
cat("Symbols needing a listing fetch:", length(symbols_needing_listing), "\n")

# Collects (symbol, date, url) rows discovered this run, used in phase 2
# alongside any already-cached listing info from prior runs.
new_report_index <- data.frame(symbol = character(0), report_date = character(0),
                                url = character(0), stringsAsFactors = FALSE)

if (length(symbols_needing_listing) > 0) {
  for (sym in symbols_needing_listing) {
    if (requests_used >= RUN_REQUEST_LIMIT) {
      cat("Run request limit reached during Phase 1. Stopping cleanly.\n")
      break
    }

    res <- getRecentReportUrls(sym)

    if (res$outcome == "ok") {
      cat(sprintf("  %-6s found %d report(s)\n", sym, length(res$urls)))
      if (length(res$urls) > 0) {
        new_report_index <- rbind(new_report_index, data.frame(
          symbol = sym, report_date = as.character(res$dates), url = res$urls,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      cat(sprintf("  %-6s listing %s\n", sym, res$outcome))
    }

    listings_manifest <- listings_manifest[listings_manifest$symbol != sym, ]
    listings_manifest <- rbind(listings_manifest, data.frame(
      symbol = sym, status = res$outcome, n_reports_found = length(res$urls),
      attempts = 1, last_try = as.character(Sys.time()), stringsAsFactors = FALSE
    ))
    saveListingsManifest(listings_manifest)
  }
}

# Persist newly discovered report URLs into the transcripts manifest as
# "pending" rows (if not already present), so Phase 2 has a full queue
# even across separate runs.
transcripts_manifest <- loadTranscriptsManifest()
if (nrow(new_report_index) > 0) {
  existing_keys <- paste(transcripts_manifest$symbol, transcripts_manifest$report_date, sep = "|")
  new_keys <- paste(new_report_index$symbol, new_report_index$report_date, sep = "|")
  to_add <- new_report_index[new_keys %in% existing_keys == FALSE, ]
  if (nrow(to_add) > 0) {
    pending_rows <- data.frame(
      symbol = to_add$symbol, report_date = to_add$report_date, url = to_add$url,
      status = "pending", file = NA, n_speech_turns = NA, attempts = 0,
      last_try = NA, stringsAsFactors = FALSE
    )
    transcripts_manifest <- rbind(transcripts_manifest, pending_rows)
    saveTranscriptsManifest(transcripts_manifest)
  }
}

# ============================================================
# PHASE 2: download transcripts for pending/retryable rows
# ============================================================

cat("\n---- Phase 2: transcript downloads ----\n")

pending <- transcripts_manifest[
  transcripts_manifest$symbol %in% symbols &
  (transcripts_manifest$status == "pending" |
   (transcripts_manifest$status %in% c("error", "empty") & transcripts_manifest$attempts < MAX_ATTEMPTS)),
]

cat("Pending/retryable transcripts:", nrow(pending), "\n")

downloaded <- 0
empties <- 0
errors <- 0

if (nrow(pending) > 0) {
  for (i in 1:nrow(pending)) {
    if (requests_used >= RUN_REQUEST_LIMIT) {
      cat("Run request limit reached during Phase 2. Stopping cleanly.\n")
      break
    }

    sym <- pending$symbol[i]
    rdate <- pending$report_date[i]
    url <- pending$url[i]
    prior_attempts <- ifelse(is.na(pending$attempts[i]), 0, pending$attempts[i])

    res <- getTranscriptFromUrl(url)

    row_key <- paste(sym, rdate, sep = "|")
    manifest_keys <- paste(transcripts_manifest$symbol, transcripts_manifest$report_date, sep = "|")

    if (res$outcome == "ok") {
      df <- res$transcript
      df$symbol <- sym
      df$report_date <- rdate
      df$source_url <- res$url
      df <- df[, c("symbol", "report_date", "speaker", "title", "msg", "source_url")]

      fname <- file.path(path.expand(OUT_DIR), paste0(sym, "_", rdate, ".csv"))
      write.csv(df, fname, row.names = FALSE)

      downloaded <- downloaded + 1
      cat(sprintf("[%d/%d] %-6s %-12s saved (%d speech turns)\n",
                  i, nrow(pending), sym, rdate, nrow(df)))

      transcripts_manifest <- transcripts_manifest[manifest_keys != row_key, ]
      transcripts_manifest <- rbind(transcripts_manifest, data.frame(
        symbol = sym, report_date = rdate, url = res$url, status = "ok",
        file = basename(fname), n_speech_turns = nrow(df),
        attempts = prior_attempts + 1, last_try = as.character(Sys.time()),
        stringsAsFactors = FALSE
      ))
    } else if (res$outcome %in% c("empty", "notfound")) {
      empties <- empties + 1
      cat(sprintf("[%d/%d] %-6s %-12s no transcript available\n", i, nrow(pending), sym, rdate))

      transcripts_manifest <- transcripts_manifest[manifest_keys != row_key, ]
      transcripts_manifest <- rbind(transcripts_manifest, data.frame(
        symbol = sym, report_date = rdate, url = url, status = "empty",
        file = NA, n_speech_turns = NA,
        attempts = prior_attempts + 1, last_try = as.character(Sys.time()),
        stringsAsFactors = FALSE
      ))
    } else {
      errors <- errors + 1
      cat(sprintf("[%d/%d] %-6s %-12s ERROR (will retry next run)\n", i, nrow(pending), sym, rdate))

      transcripts_manifest <- transcripts_manifest[manifest_keys != row_key, ]
      transcripts_manifest <- rbind(transcripts_manifest, data.frame(
        symbol = sym, report_date = rdate, url = url, status = "error",
        file = NA, n_speech_turns = NA,
        attempts = prior_attempts + 1, last_try = as.character(Sys.time()),
        stringsAsFactors = FALSE
      ))
    }

    saveTranscriptsManifest(transcripts_manifest)
  }
}

# ============================================================
# SUMMARY
# ============================================================

cat("\n---- Run summary ----\n")
cat("Requests used this run:", requests_used, "of", RUN_REQUEST_LIMIT, "\n")
cat("Transcripts saved:", downloaded, "\n")
cat("No transcript available:", empties, "\n")
cat("Transient errors (retry next run):", errors, "\n")

full_manifest <- loadTranscriptsManifest()
if (nrow(full_manifest) > 0) {
  total_ok <- sum(full_manifest$status == "ok")
  total_pending <- sum(full_manifest$status %in% c("pending", "error"))
  cat("\nTotal transcripts on disk:", total_ok, "\n")
  cat("Still pending/retryable:", total_pending, "\n")
  cat("Output folder:", path.expand(OUT_DIR), "\n")
}
cat("Run this script again to continue.\n")
cat("---------------------\n")
