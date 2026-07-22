# ============================================================
# FIN_Z_refresh_listings_marketbeat.R
# INSTRUCTOR ONLY. Not for student use.
#
# Purpose: unlock stale symbols so FIN_Z_download_transcripts_marketbeat.R
# will rediscover their newer earnings quarters.
#
# FIN_Z_download_transcripts_marketbeat.R treats a symbol as
# permanently done once its MarketBeat earnings-tab listing has
# been fetched once (status "ok" or "empty" in
# _manifest_marketbeat_listings.csv). That's fine within a single
# pass, but it means running that script again next year finds
# "0 symbols needing a listing fetch" and downloads nothing new,
# even though 4 fresh quarters exist on the live site by then.
#
# This script does not touch FIN_Z_download_transcripts_marketbeat.R,
# any saved transcript CSVs, or the transcripts manifest. It only
# removes old rows from the LISTINGS manifest so that script's own
# "symbols needing a listing fetch" check picks them back up on its
# next run. New report dates it finds get added as new pending rows
# and downloaded as new files; already-downloaded dates are skipped
# automatically by that script's existing dedup logic, so nothing
# gets re-fetched or overwritten.
#
# Usage:
#   1. Run this script (defaults to a dry run, see DRY_RUN below).
#   2. Review what it says it would reset.
#   3. Set DRY_RUN <- FALSE and run again to actually apply it.
#   4. Run FIN_Z_download_transcripts_marketbeat.R as usual (possibly
#      several times, same as the first pass) to pull the new quarters.
# ============================================================

# ============================================================
# CONFIGURATION
# ============================================================

ARCHIVE_DIR <- "~/Desktop/vienna-genai-finance-course/earnings_call_archive"
LISTINGS_MANIFEST_FILE <- file.path(ARCHIVE_DIR, "_manifest_marketbeat_listings.csv")

# A symbol's listing is considered stale (and gets reset) once its
# last_try is older than this many days. 180 days comfortably covers
# a "check back once or twice a year" cadence: new quarterly earnings
# post roughly every ~90 days, so this catches at least one missed
# quarter without re-scraping the whole index every run.
REFRESH_OLDER_THAN_DAYS <- 180

# Optional: only refresh specific symbols regardless of age (e.g. one
# ticker you know reported since the last pull). NA means age is the
# only criterion.
FORCE_REFRESH_SYMBOLS <- NA

# Safety default: show what would change without writing anything.
# Flip to FALSE once you've reviewed the printed list.
DRY_RUN <- TRUE

# ============================================================
# RUN
# ============================================================

mpath <- path.expand(LISTINGS_MANIFEST_FILE)
if (file.exists(mpath) == FALSE) {
  stop(paste("Listings manifest not found at:", mpath,
             "- nothing to refresh yet. Run FIN_Z_download_transcripts_marketbeat.R first."))
}

listings_manifest <- read.csv(mpath, stringsAsFactors = FALSE)
cat("Loaded", nrow(listings_manifest), "symbol listing(s) from", basename(mpath), "\n")

age_days <- as.numeric(difftime(Sys.time(), as.POSIXct(listings_manifest$last_try), units = "days"))

is_stale <- is.na(age_days) | age_days >= REFRESH_OLDER_THAN_DAYS

is_forced <- rep(FALSE, nrow(listings_manifest))
if (length(FORCE_REFRESH_SYMBOLS) > 1 || is.na(FORCE_REFRESH_SYMBOLS[1]) == FALSE) {
  is_forced <- listings_manifest$symbol %in% FORCE_REFRESH_SYMBOLS
}

to_reset <- is_stale | is_forced

cat("\n---- Refresh plan ----\n")
cat("Stale (older than", REFRESH_OLDER_THAN_DAYS, "days):", sum(is_stale), "\n")
cat("Force-refreshed by symbol list:", sum(is_forced & !is_stale), "\n")
cat("Total symbols to unlock:", sum(to_reset), "of", nrow(listings_manifest), "\n")

if (sum(to_reset) > 0) {
  preview <- listings_manifest[to_reset, c("symbol", "status", "last_try")]
  preview$age_days <- round(age_days[to_reset], 1)
  cat("\nSymbols that will be unlocked:\n")
  print(head(preview, 25), row.names = FALSE)
  if (nrow(preview) > 25) {
    cat("... and", nrow(preview) - 25, "more\n")
  }
}

if (DRY_RUN == TRUE) {
  cat("\nDRY_RUN is TRUE: no changes written.\n")
  cat("Set DRY_RUN <- FALSE and run again to apply this reset.\n")
} else if (sum(to_reset) == 0) {
  cat("\nNothing to unlock. Manifest left unchanged.\n")
} else {
  listings_manifest <- listings_manifest[to_reset == FALSE, ]
  write.csv(listings_manifest, mpath, row.names = FALSE)
  cat("\nUnlocked", sum(to_reset), "symbol(s). They will be re-checked the next time",
      "FIN_Z_download_transcripts_marketbeat.R runs.\n")
  cat("Remaining resolved symbols in manifest:", nrow(listings_manifest), "\n")
}
