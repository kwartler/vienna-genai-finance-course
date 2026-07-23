# ============================================================
# FIN_B_macro_risk.R
# LLM Context lesson, Day 2, Finance Masters track
# (8:00 to 9:00)
#
# Purpose: pull live macro and geopolitical risk alerts, and
# think about what belongs in a context window and what is
# just noise.
#
# How to use this script: run it a block at a time and read
# what prints.
#
# This is your first API call of the day and it needs NO KEY.
# Riskline publishes these alerts openly, so nothing can go
# wrong with authentication. In FIN_C we add a key.
# ============================================================

# ---- Libraries ----
# httr:     makes the web request
# jsonlite: turns the JSON response into an R object
library(httr)
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

RISKLINE_URL <- "https://api.riskline.com/alerts/latest.json"

# Countries you care about for a given investment thesis.
# Change these and re-run to see the filter work.
COUNTRIES_OF_INTEREST <- c("United States", "China", "Taiwan", "Germany")

# ============================================================
# FETCH THE ALERTS
# ============================================================

cat("Requesting the latest Riskline alerts.\n")

response <- GET(RISKLINE_URL, timeout(30))

# Always check the response before trusting it. A failed
# request still returns an object, it just has no useful data
# inside, and a script that skips this check will fail later
# in a confusing place.
if (status_code(response) != 200) {
  stop(paste("Request failed with HTTP status", status_code(response)))
} else {
  cat("Request succeeded.\n")
}

raw_text <- content(response, as = "text", encoding = "UTF-8")
alerts_parsed <- fromJSON(raw_text, flatten = TRUE)

# The API wraps its results, so find the actual table of
# alerts whatever the wrapper happens to be called.
if (is.data.frame(alerts_parsed) == TRUE) {
  alerts <- alerts_parsed
} else {
  alerts <- NULL
  for (nm in names(alerts_parsed)) {
    if (is.data.frame(alerts_parsed[[nm]]) == TRUE) {
      alerts <- alerts_parsed[[nm]]
      cat("Alerts found under the field named:", nm, "\n")
      break
    }
  }
}

if (is.null(alerts) == TRUE) {
  cat("\nCould not find a table of alerts. Here is the raw structure:\n")
  str(alerts_parsed, max.level = 2)
  stop("Inspect the structure above and adjust this script.")
} else {
  cat("Alerts received:", nrow(alerts), "\n\n")
}

# ============================================================
# WHAT DID WE ACTUALLY GET?
# ============================================================

cat("---- Fields available ----\n")
cat(paste(names(alerts), collapse = ", "), "\n\n")

# TIP: look at that field list before writing any more code.
# Real APIs rarely return what you assumed. Reading the shape
# of a response first is the habit that saves the most time.

cat("---- First few alerts ----\n")
preview_cols <- intersect(c("country", "title", "category", "risk_level", "start_date"), names(alerts))

if (length(preview_cols) > 0) {
  print(head(alerts[, preview_cols], 8), row.names = FALSE)
} else {
  print(head(alerts, 3))
}
cat("\n")

# ============================================================
# FILTER TO WHAT MATTERS
# ============================================================

# This is the context lesson. A global risk feed contains
# alerts about dozens of countries. Nearly all of them are
# irrelevant to any single investment thesis. Sending all of
# them to a language model costs money and, worse, buries the
# relevant signal in noise.

if ("country.name" %in% names(alerts) == TRUE) {
  relevant <- alerts[alerts$country %in% COUNTRIES_OF_INTEREST, ]
  cat("---- Filtered to your countries of interest ----\n")
  cat("Before:", nrow(alerts), "alerts\n")
  cat("After: ", nrow(relevant), "alerts\n")
  if (nrow(alerts) > 0) {
    cat("Kept:  ", round(100 * nrow(relevant) / nrow(alerts)), "percent\n\n")
  }

  if (nrow(relevant) > 0) {
    show_cols <- intersect(c("country", "title", "category"), names(relevant))
    print(head(relevant[, show_cols], 10), row.names = FALSE)
    cat("\n")
  } else {
    cat("No current alerts for those countries. Try adding others.\n\n")
  }
} else {
  cat("This response has no 'country' field, so the filter is skipped.\n")
  cat("Look at the field list above and pick something else to filter on.\n\n")
  relevant <- alerts
}

# ============================================================
# TURN IT INTO CONTEXT
# ============================================================

# A language model cannot read a data frame. Context has to
# be text. Here we compress the filtered alerts into a short
# block that would sit inside a prompt.

buildMacroContext <- function(alert_rows) {
  if (nrow(alert_rows) == 0) {
    return("No significant macro risk alerts for the regions of interest.")
  }

  lines <- character(0)
  n_show <- min(nrow(alert_rows), 10)

  for (i in 1:n_show) {
    country_txt <- ifelse("country" %in% names(alert_rows) == TRUE, alert_rows$country[i], "Unknown")
    title_txt <- ifelse("title" %in% names(alert_rows) == TRUE, alert_rows$title[i], "")
    lines <- c(lines, paste0("- ", country_txt, ": ", title_txt))
  }

  block <- paste0("CURRENT MACRO RISK ALERTS\n", paste(lines, collapse = "\n"))
  return(block)
}

macro_context <- buildMacroContext(relevant)

cat("---- The context block ----\n")
cat(macro_context, "\n\n")

cat("Size of this block:", nchar(macro_context), "characters\n")
cat("Roughly", round(nchar(macro_context) / 4), "tokens\n\n")

# TIP: compare that number to the transcript size from FIN_A.
# Macro risk is tiny. It is cheap to include and it tells the
# model something it could not possibly infer from a company's
# own earnings call. That combination, high value and low
# cost, is exactly what you want in a context window.

# ============================================================
# SAVE IT
# ============================================================

# FIN_D assembles this together with the transcript and news
# into a single prompt.
#####
saveRDS(macro_context, "macro_context.rds")
cat("Saved macro_context.rds for use in FIN_D.\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Summary ----\n")
cat("1. Not every API needs a key. Start with the easy ones.\n")
cat("2. Read the shape of a response before writing code against it.\n")
cat("3. Filtering is context engineering. Noise costs money and hides signal.\n")
cat("4. Context must be text, so structured data has to be rendered into prose.\n")

