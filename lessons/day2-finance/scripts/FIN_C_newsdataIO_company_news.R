# ============================================================
# FIN_C_newsdataIO_company_news.R
# LLM Context lesson, Day 2, Finance Masters track
#
# Purpose: pull recent news about a company, then enrich it with
# a search model, and save a context block for the next step in
# the pipeline (FIN_D).
#
# Two skills at once: how to handle an API key safely, and how
# to ask a search API a precise question. Then a third: chaining
# one API into another.
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

api_key <- Sys.getenv("NEWS_DATA_IO_API_KEY")

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

# The "latest" endpoint always covers the past 48 hours. If you
# need a custom date range you have to use the paid archive
# endpoint (/api/1/archive) instead.
#
# The free plan returns up to 10 articles per request. We cap
# in R so this stays correct no matter what plan the key is on.
MAX_ARTICLES <- 10

# ---- Enrichment step (second half of this script) ----
# newsdata.io gives us headlines and short descriptions but no
# full article text on the free plan. To get depth we hand those
# headlines to a model that can search the web and cite what it
# finds.
OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"

# perplexity/sonar is cheap (about $1 per million tokens, with
# search included) and returns citations, which is why we use it.
ENRICH_MODEL <- "perplexity/sonar"

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
status_code(response)

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
# enough to do deep analysis of HOW it was reported. This is
# exactly the gap the enrichment step below fills.
if ("content" %in% names(articles)) {
  cat("\nExample of the content field (free plan is a placeholder):\n")
  cat(articles$content[1], "\n")
}

# ============================================================
# TURN IT INTO CONTEXT
# ============================================================

# We are not calling an LLM here. We are assembling a plain-text
# context block from structured data. Gathering and formatting
# is not a language task, so it stays in R.
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
# SAVE THE HEADLINE-ONLY VERSION
# ============================================================

pth_headlines <- file.path(savePth, "news_context_headlines.rds")
saveRDS(news_context, pth_headlines) # WILL OVERWRITE OLD NEWS CONTEXT
cat(paste0("\nSaved news_context_headlines.rds (the headline-only version) in\n"),
    pth_headlines, "\n")

# ============================================================
# ENRICHMENT: FILL IN WHAT NEWSDATA WOULD NOT GIVE US
# ============================================================

# We have breadth (many sources, dated, attributed) but no
# depth (no full body text on the free plan). So we take the
# headlines we just gathered and hand them to a model that CAN
# search the web and read what it finds.
#
# This is the pattern worth learning: use one API to work out
# what to ask, then use a second to answer it properly.
# newsdata.io is the better tool for "what happened and who
# reported it," because it is structured and cheap. A search
# model is the better tool for "what does it mean," because it
# can read the actual articles and synthesize them.
#
# Note what the model is and is not doing. It is reading and
# summarizing, which is a language task. It is not being asked
# to invent facts or compute anything. And because it returns
# citations, every claim can be traced back to a source.
#
# We make ONE call with the whole headline list, not one call
# per article. That is cheaper but may not be optimal and gives the
# model the full picture to reason over at once.

openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

# ---- Build the query FROM the headlines ----
# This is the hinge of the whole script. We are not asking a
# vague question. We are telling the search model exactly which
# stories we already know exist, so it goes and reads those
# rather than wandering.
headlines_block <- paste0(
  articles$source_display, " | ",
  articles$pubDate, " | ",
  articles$title,
  collapse = "\n"
)

user_prompt <- paste0(
  "I am researching ", COMPANY_NAME, " (", SYMBOL, ") as an investment.\n\n",
  "A news API returned these recent headlines:\n",
  headlines_block, "\n\n",
  "Search for and read the underlying news coverage, then give me a briefing that covers:\n",
  "1. What actually happened in each significant story, in a sentence or two.\n",
  "2. Any financial figures reported (revenue, margins, guidance, analyst targets).\n",
  "3. What is disputed or uncertain, where sources disagree.\n\n",
  "Be concise and factual. Do not speculate beyond what the sources say. ",
  "If something is unclear or unreported, say so."
)

# Look at the prompt we are about to send.
cat("\n---- Enrichment prompt ----\n")
cat(user_prompt, "\n")

system_prompt <- paste0(
  "You are a research assistant for an investment analyst. ",
  "You summarize what published sources report. You do not give investment advice, ",
  "and you do not state anything you cannot attribute to a source.")

# Start from the headline context. If enrichment cannot run or
# fails, this is what we save, so the pipeline never breaks.
enriched_context <- news_context

if (nchar(openrouter_key) == 0) {
  cat("\nNo OPENROUTER_API_KEY found. Skipping enrichment.\n")
  cat("news_context.rds will hold the headline-only version.\n")
} else {
  
  # Organize the call
  request_body <- list(
    model = ENRICH_MODEL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    )
  )
  
  # Make the request
  enrich_response <- POST(
    OPENROUTER_URL,
    add_headers(
      "Authorization" = paste("Bearer", openrouter_key),
      "Content-Type"  = "application/json"
    ),
    body = toJSON(request_body, auto_unbox = TRUE),
    timeout(60)
  )
  
  # Check the status and print any issues
  if (status_code(enrich_response) == 200) {
    cat("\nEnrichment succeeded.\n\n")
    
    parsed_enrich   <- content(enrich_response)
    message_content <- parsed_enrich$choices[[1]]$message$content
    
    if (is.null(message_content)) {
      cat("Enrichment returned no message content. Keeping the headline version.\n")
    } else {
      
      # Pull the citation URLs out of the annotations, if any.
      annotations   <- parsed_enrich$choices[[1]]$message$annotations
      sources_block <- ""
      if (is.null(annotations) == FALSE && length(annotations) > 0) {
        urls <- unlist(lapply(annotations, function(ann) {
          if (is.null(ann$type) == FALSE && ann$type == "url_citation") {
            ann$url_citation$url
          } else {
            NULL
          }
        }))
        if (length(urls) > 0) {
          sources_block <- paste0("SOURCES:\n", paste(unique(urls), collapse = "\n"))
        }
      }
      
      briefing <- paste0(
        "BACKGROUND INFORMATION COMPILED FROM THE WEB:\n\n",
        message_content, "\n\n",
        sources_block
      )
      
      enriched_context <- paste0(
        news_context, "\n\n",
        "BACKGROUND BRIEFING (compiled from web sources)\n",
        briefing
      )
    }
    
  } else {
    cat("\nEnrichment failed with HTTP status", status_code(enrich_response), "\n")
    cat("Keeping the headline-only version so the pipeline still has its file.\n")
  }
}

# ============================================================
# SAVE THE CONTEXT FILE THE PIPELINE READS
# ============================================================

# Same name and location as before, so FIN_D and later scripts
# keep working whether or not enrichment ran.
pth <- file.path(savePth, "news_context.rds")
saveRDS(enriched_context, pth) # WILL OVERWRITE OLD NEWS CONTEXT from previous runs
cat("\nSaved news_context.rds (for use in FIN_D) in\n", pth, "\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Learning Review ----\n")
cat("1. Keys live in the environment, never in the script.\n")
cat("2. A vague query returns vague results. Be specific on purpose.\n")
cat("3. Know what your plan actually returns. The latest endpoint covers 48 hours and the free plan withholds full content.\n")
cat("4. Chaining APIs is a common practice: one API decides what to ask, another answers it well.\n")
cat("5. Prefer a model that cites its sources, so its claims can be checked.\n")
cat("6. Always leave a working fallback. If enrichment fails, the pipeline still gets its context file.\n")

# End