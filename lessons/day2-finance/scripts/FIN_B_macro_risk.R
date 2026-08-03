# ============================================================
# FIN_B_macro_risk.R
# LLM Context lesson, Day 2, Finance Masters track
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

# Open this in the browser to see it live
RISKLINE_URL <- "https://api.riskline.com/alerts/latest.json"

# Save Path
### CREATE a folder in your repo called `context_files` then
### Change the path to your folder
savePth <- '~/Desktop/vienna-genai-finance-course/context_files'

# Countries you care about for a given investment thesis.
# Change these and re-run to see the filter work.
COUNTRIES_OF_INTEREST <- c("United States of America", 
                           "China", "Taiwan", "Germany")

# ============================================================
# FETCH THE ALERTS
# ============================================================

cat("Requesting the latest Riskline alerts.\n")
response <- GET(RISKLINE_URL, timeout(30))

# Always check the response before trusting it. A failed
# request still returns an object, it just has no useful data
# inside, and a script that skips this check will fail later
# in a confusing place.
# We are looking for a "status 200" anything else is an issue
if (status_code(response) != 200) {
  stop(paste("Request failed with HTTP status", status_code(response)))
} else {
  cat("Request succeeded.\n")
}

# See the result back in R as "response object"
response
content(response) #lots of different components returned

# Extract the content of the returned data more easily
raw_text      <- content(response, as = "text", encoding = "UTF-8")
alerts_parsed <- fromJSON(raw_text, flatten = TRUE)

# The API wraps its results in a field called "alerts", so the
# table we want is one level down.
alerts <- alerts_parsed$alerts

if (is.null(alerts) == TRUE || is.data.frame(alerts) == FALSE) {
  cat("\nExpected a table under $alerts but did not find one.\n")
  cat("Here is the actual structure returned:\n")
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
head(alerts)

# TIP: look at that field list before writing any more code.
# Real APIs rarely return what you assumed. Reading the shape
# of a response first is the habit that saves the most time.

cat("---- First few alerts ----\n")
# The fields we care about are the country name, the headline,
# and when it was published. flag_path is a link to an image
# and is no use to a language model, so we leave it out.
preview_cols <- intersect(c("country.name", "title", "created_at"), names(alerts))

if (length(preview_cols) > 0) {
  print(head(alerts[, preview_cols], 8), row.names = FALSE)
  } else {
  print(head(alerts, 3))
    }

# ============================================================
# FILTER TO WHAT MATTERS
# ============================================================

# A global risk feed contains alerts about dozens of countries. 
# Nearly all of them are irrelevant to any single investment thesis. 
# Sending all of them to a language model costs money and, 
# buries the relevant signal in noise.

# Before filtering, see which countries actually appear in this real time GET response.

cat("Countries present in this feed:\n")
t(t(table(alerts$country.name)))
  
# Assuming out interest countries are in the data pull
relevant <- alerts[alerts$country.name %in% COUNTRIES_OF_INTEREST, ]

cat("Original Data:", nrow(alerts), "alerts\n")
cat("Filtered Data:", nrow(relevant), "alerts\n")

if(nrow(relevant)!=0){
  relevant$title
  } else {
  print('No current alerts for countries of interest.  Review the table of country names')
    }


# ============================================================
# TURN IT INTO CONTEXT
# ============================================================

# A language model cannot read a data frame. Context has to
# be text. Here we compress the filtered alerts into a short
# block that would sit inside a prompt
buildMacroContext <- function(risk_alerts){
  # Check we have existing alerts
  if(nrow(risk_alerts)==0){
    macroBlock <- 'No significant macro risk alerts for the regions of interest.'
  } else {
    macroBlock <- apply(risk_alerts[,1:2],1, paste0, collapse = '\n')
    macroBlock <- paste(macroBlock, collapse = '\n')
  }
  macroBlock <- paste('CURRENT MACRO RISK ALERTS\n',
                      macroBlock,
                      collapse = '\n')
 return(macroBlock) 
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
#
# Also notice how little this feed actually carries: a title,
# a timestamp, a country, and a link to a flag image. There is
# no body text and no severity score. The headline IS the
# alert. That is a limitation worth knowing before you build
# anything on top of it, and it is the kind of thing you only
# discover by printing the field names first.

# ============================================================
# SAVE IT
# ============================================================

# FIN_D assembles this together with the transcript and news
# into a single prompt.
pth <- file.path(savePth, "macro_context.rds") # THIS WILL OVERWRITE OLD MACRO RDS
saveRDS(macro_context, pth)
cat(paste("Saved macro_context.rds for use in FIN_D.\n in", pth))

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Learning Review ----\n")
cat("1. Not every API needs a key. Start with the easy ones.\n")
cat("2. Read the shape of a response before writing code against it.\n")
cat("3. Filtering is context engineering. Noise costs money and hides signal.\n")
cat("4. Context must be text, structured data has to be adjusted as prose or JSON because LLMs can struggle with large tabular data.\n")
