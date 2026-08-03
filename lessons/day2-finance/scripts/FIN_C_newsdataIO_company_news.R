# ============================================================
# FIN_C_newsdataIO_company_news.R
# LLM Context lesson, Day 2, Finance Masters track
#
# Purpose: pull recent news about a company and turn it into a
# clean context block we can save for the next step in the
# pipeline (FIN_D). This is a SIMPLE, single-request build.
#
# Two skills at once: how to handle an API key safely, and how
# to ask a search API a precise question.
#
# How to use this script: run it a block at a time and read
# what prints.
#
# FIN_B needed no key. This one does. That difference is the
# first thing we deal with below.
# ============================================================

# ---- Libraries ----
library(httr)
library(jsonlite)

# ============================================================
# THE API KEY
# ============================================================

# Get a free key at https://newsdata.io/register
#
# Set it in the CONSOLE, not in this file:
#   usethis::edit_r_environ()
#   OR
#   Sys.setenv(NEWSDATA_API_KEY = "your_key_here")
#
# WHY THIS MATTERS. A key is a password. If you type it into a
# script and that script goes into a shared repo, you have
# published your password. This is one of the most common
# real-world security mistakes, and people who know better
# commit it constantly. Reading the key from the environment
# costs you one extra line and removes the risk entirely.

api_key <- Sys.getenv("NEWSDATA_API_KEY")

if (nchar(api_key) == 0) {
  stop("No API key found. Run Sys.setenv(NEWSDATA_API_KEY = \"your_key_here\") in the console first.")
} else {
  cat("API key found. Length:", nchar(api_key), "characters\n")
}

# ============================================================
# CONFIGURATION
# ============================================================

# newsdata.io "latest" endpoint (keyword search over recent news).
NEWSDATA_URL <- "https://newsdata.io/api/1/latest"

# Save path.
# Create a folder in your repo called `context_files`.
savePth <- "~/Desktop/vienna-genai-finance-course/context_files"

# The company to research.
SYMBOL       <- "AAPL"
COMPANY_NAME <- "Apple"

# The "latest" endpoint always covers the past 48 hours. That
# window is fixed, so there is no from_date to set here. If you
# need a custom date range you have to use the paid archive
# endpoint (/api/1/archive) instead.
#
# The free plan returns up to 10 articles per request. We cap
# in R so this stays correct no matter what plan the key is on.
MAX_ARTICLES <- 10

# ============================================================
# BUILDING A GOOD QUERY
# ============================================================

# A search API answers exactly what you ask. Quotes force an
# exact phrase, AND requires both terms, OR widens the net.
# newsdata.io expects the operators in uppercase.
query <- paste0("\"",
                COMPANY_NAME,
                "\" AND (earnings OR revenue OR stock OR shares OR analyst)")

cat("\nQuery being sent:\n", query, "\n\n")

# ============================================================
# MAKE THE REQUEST
# ============================================================

cat("Requesting the latest news (past 48 hours).\n")

response <- GET(NEWSDATA_URL,
                query = list(
                  q        = query,
                  language = "en",
                  apikey   = api_key
                ),
                timeout(30))

# Did we get 200?
code <- status_code(response)

if (code != 200) {
  err_txt <- content(response, as = "text", encoding = "UTF-8")
  cat("\nRequest failed with HTTP status", code, "\n")
  cat("The API said:\n", err_txt, "\n\n")

  if (code == 401) {
    cat("401 means the key was rejected. Check NEWSDATA_API_KEY.\n")
  }
  if (code == 422) {
    cat("422 usually means a parameter your plan does not allow, or a value it does not accept.\n")
  }
  if (code == 429) {
    cat("429 means you hit the rate limit. Wait, then try again.\n")
  }
  stop("Stopping. See the message above.")
} else {
  cat("Request succeeded.\n")
}

# It succeeded, so extract the response.
raw_text <- content(response, as = "text", encoding = "UTF-8")
parsed   <- fromJSON(raw_text, flatten = TRUE)

# newsdata.io also carries a status field in the body. A 200
# with status "error" is still an error, so check it.
if (parsed$status != "success") {
  cat("The API returned status:", parsed$status, "\n")
  stop("Stopping. The response body reported an error.")
}

# newsdata.io wraps the results as `totalResults` plus a
# `results` table. Note the field names differ from other news
# APIs. We assign it to `articles` so the rest of the script
# reads naturally.
cat("Total results available:", parsed$totalResults, "\n")
cat("Total returned in this call:", nrow(parsed$results), "\n")

articles <- parsed$results

# Cap to MAX_ARTICLES so behaviour is predictable across plans.
if (nrow(articles) > MAX_ARTICLES) {
  articles <- head(articles, MAX_ARTICLES)
}

# newsdata.io gives source_name (human readable) on most
# articles and source_id (a slug) on all of them. Prefer the
# readable one, fall back to the slug.
if ("source_name" %in% names(articles)) {
  articles$source_display <- ifelse(
    is.na(articles$source_name) | articles$source_name == "",
    articles$source_id,
    articles$source_name
  )
} else {
  articles$source_display <- articles$source_id
}

# ============================================================
# WHAT DID WE GET?
# ============================================================

cat("\n---- Fields available ----\n")
print(names(articles))

cat("\n---- Headlines ----\n")
show_cols <- intersect(c("pubDate", "source_display", "title"), names(articles))
preview   <- articles[, show_cols]
print(head(preview, 3), row.names = FALSE)

# ============================================================
# THE LIMITATION YOU SHOULD KNOW ABOUT
# ============================================================

# On the free plan the `content` field is not the full article.
# newsdata.io returns a paid-plan placeholder there. So what you
# actually have per article is a headline and a one-line
# description. That is enough to know WHAT happened. It is not
# enough to do deep analysis of HOW it was reported.
if ("content" %in% names(articles)) {
  cat("\nExample of the content field (free plan is a placeholder):\n")
  cat(articles$content[1], "\n")
}

# ============================================================
# TURN IT INTO CONTEXT
# ============================================================

# We are not calling an LLM here. We are assembling a plain-text
# context block from structured data. The model reads this
# later, in FIN_D. Gathering and formatting is not a language
# task, so it stays in R.
buildNewsContext <- function(df, company) {
  if (nrow(df) == 0) {
    block <- paste0("No recent news found for ", company, ".")
  } else {
    allArticle <- list()
    for (i in 1:nrow(df)) {
      oneArticle <- df[i, ]
      x <- paste0("Source Name: ", oneArticle$source_display, "\n",
                  "Published At: ", oneArticle$pubDate, "\n",
                  "Article Title: ", oneArticle$title, "\n",
                  "Article Description: ", oneArticle$description, "\n",
                  collapse = "\n")
      allArticle[[i]] <- x
    }
    block <- paste0(unlist(allArticle), collapse = "\n\n")
  }

  return(block)
}

news_context <- buildNewsContext(articles, COMPANY_NAME)

cat("\nSize of this block:", nchar(news_context), "characters\n")
cat("Roughly", round(nchar(news_context) / 4), "tokens\n\n")
cat(news_context)

# ============================================================
# SAVE IT
# ============================================================

# This is the file FIN_D reads.
pth <- file.path(savePth, "news_context.rds")
saveRDS(news_context, pth) # WILL OVERWRITE OLD NEWS CONTEXT
cat(paste0("\nSaved news_context.rds in\n"), pth, "\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Learning Review ----\n")
cat("1. Keys live in the environment, never in the script.\n")
cat("2. A vague query returns vague results. Be specific on purpose.\n")
cat("3. Know what your plan actually returns. The latest endpoint covers 48 hours and the free plan withholds full content.\n")
cat("4. Read the response fields carefully. newsdata.io uses results and totalResults, and pubDate, not the names other APIs use.\n")
cat("5. Formatting news into context is an R job, not an LLM job. The model reads this later.\n")
