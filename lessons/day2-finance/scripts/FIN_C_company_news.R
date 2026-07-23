# ============================================================
# FIN_C_company_news.R
# LLM Context lesson, Day 2, Finance Masters track
# (8:00 to 9:00)
#
# Purpose: pull recent news about a company, and learn two
# things at once: how to handle an API key safely, and how to
# ask a search API a precise question.
#
# How to use this script: run it a block at a time and read
# what prints.
#
# FIN_B needed no key. This one does. That difference is the
# whole first half of this script.
# ============================================================

# ---- Libraries ----
library(httr)
library(jsonlite)

# ============================================================
# THE API KEY
# ============================================================

# Get a free key at https://newsapi.org/register
#
# Set it in the CONSOLE, not in this file:
#   Sys.setenv(NEWSAPI_KEY = "your_key_here")
#
# WHY THIS MATTERS. A key is a password. If you type it into
# a script and that script goes into a shared repo, you have
# published your password. This is one of the most common
# real-world security mistakes, and it is committed by people
# who know better, constantly. Reading the key from the
# environment costs you one extra line and removes the risk
# entirely.

api_key <- Sys.getenv("NEWSORG_API_KEY")

if (nchar(api_key) == 0) {
  stop("No API key found. Run Sys.setenv(NEWSAPI_KEY = \"your_key_here\") in the console first.")
} else {
  cat("API key found. Length:", nchar(api_key), "characters\n")
}

# TIP: notice we printed the LENGTH, not the key. When you are
# debugging, print enough to confirm something loaded and
# nothing more. Never cat() a secret to a console that might
# be on a projector.

# ============================================================
# CONFIGURATION
# ============================================================

NEWS_URL <- "https://newsapi.org/v2/everything"

# Save Path
savePth <- '~/Desktop/vienna-genai-finance-course/context_files'

# The company to research.
SYMBOL <- "AAPL"
COMPANY_NAME <- "Apple"

# How far back to look. The free plan only serves articles
# from roughly the last month, so do not ask for more.
DAYS_BACK <- 14

# How many articles to request. Max is 100 on this endpoint.
PAGE_SIZE <- 20

# ---- Enrichment step (second half of this script) ----
# NewsAPI gives us headlines but no article text. To get depth
# we hand those headlines to a model that can search the web
# and cite what it finds.
OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Kept as a top level constant so it is a one line change.
# perplexity/sonar is cheap (about $1 per million tokens, with
# search included) and returns citations, which is why we use
# it here.
ENRICH_MODEL <- "perplexity/sonar"

# Set to FALSE to skip the enrichment step entirely and run
# only the NewsAPI half of this script.
DO_ENRICHMENT <- TRUE

# ============================================================
# BUILDING A GOOD QUERY
# ============================================================

# A search API answers exactly what you ask, which is the
# problem. Consider three ways to ask about this company:
#
#   AAPL            -> few results, ticker rarely in prose
#   Apple           -> fruit, Apple Records, apple pie recipes
#   "Apple" + terms -> the company, in a financial context
#
# Quotation marks force an exact phrase. AND requires both
# terms. We combine the company name with finance words so
# that a story has to be about the business, not the fruit.

query <- paste0("\"", COMPANY_NAME, "\" AND (earnings OR revenue OR stock OR shares OR analyst)")

cat("\nQuery being sent:\n", query, "\n\n")

from_date <- format(Sys.Date() - DAYS_BACK, "%Y-%m-%d")

# ============================================================
# MAKE THE REQUEST
# ============================================================

cat("Requesting news from", from_date, "onward.\n")

response <- GET(NEWS_URL,
                query = list(
                  q = query,
                  from = from_date,
                  language = "en",
                  sortBy = "publishedAt",
                  pageSize = PAGE_SIZE,
                  apiKey = api_key
                ),
                timeout(30))

# ---- Read the status before touching the data ----
# This API explains its own failures clearly, so print what it
# said rather than guessing.
code <- status_code(response)

if (code != 200) {
  err_txt <- content(response, as = "text", encoding = "UTF-8")
  cat("\nRequest failed with HTTP status", code, "\n")
  cat("The API said:\n", err_txt, "\n\n")

  if (code == 401) {
    cat("401 means the key was rejected. Check NEWSAPI_KEY.\n")
  }
  if (code == 429) {
    cat("429 means you hit the daily limit (100 requests on the free plan).\n")
  }
  if (code == 426) {
    cat("426 usually means you asked for articles older than the free plan allows.\n")
    cat("Try reducing DAYS_BACK.\n")
  }
  stop("Stopping. See the message above.")
} else {
  cat("Request succeeded.\n")
}

raw_text <- content(response, as = "text", encoding = "UTF-8")
parsed <- fromJSON(raw_text, flatten = TRUE)

# The response wraps the results: status, totalResults, and
# then the articles table.
cat("Status field:", parsed$status, "\n")
cat("Total results available:", parsed$totalResults, "\n")

articles <- parsed$articles

if (is.null(articles) == TRUE || nrow(articles) == 0) {
  stop("No articles came back. Try a broader query or a longer DAYS_BACK.")
} else {
  cat("Articles returned in this page:", nrow(articles), "\n\n")
}

# TIP: totalResults is how many exist. nrow(articles) is how
# many you actually received. They are almost never the same,
# because results arrive in pages. Confusing the two is a
# classic source of "why is my data incomplete?"

# ============================================================
# WHAT DID WE GET?
# ============================================================

cat("---- Fields available ----\n")
cat(paste(names(articles), collapse = ", "), "\n\n")

# flatten = TRUE turned the nested source object into
# source.id and source.name, the same way FIN_B turned
# country into country.name.

cat("---- Headlines ----\n")
show_cols <- intersect(c("publishedAt", "source.name", "title"), names(articles))
preview <- articles[, show_cols]
preview$publishedAt <- substr(preview$publishedAt, 1, 10)
preview$title <- substr(preview$title, 1, 70)
print(head(preview, 10), row.names = FALSE)
cat("\n")

# ============================================================
# THE LIMITATION YOU MUST KNOW ABOUT
# ============================================================

# The free plan does NOT give you article text. The `content`
# field is cut off at 200 characters. Look at it yourself
# rather than taking my word for it.

cat("---- How much article text do we actually get? ----\n")

content_lengths <- nchar(articles$content)
content_lengths[is.na(content_lengths) == TRUE] <- 0

cat("Longest content field:", max(content_lengths), "characters\n")
cat("Median content field: ", median(content_lengths), "characters\n\n")

cat("Here is one, in full:\n")
cat("\"", articles$content[1], "\"\n\n", sep = "")

# STOP AND READ THAT.
# It ends mid-sentence with something like [+2431 chars]. That
# is the API telling you exactly how much it withheld.
#
# So what you actually have per article is: a headline, a
# one-line description, and a 200 character stub. That is
# enough to know WHAT happened. It is not enough to do deep
# analysis of HOW it was reported.
#
# This is why the earnings call transcripts matter. Those you
# have in full. News gives you breadth (many sources, current
# events). Transcripts give you depth (complete text, direct
# quotes). A good context window often needs both, for
# different reasons.

# ============================================================
# FILTER FOR RELEVANCE
# ============================================================

# Even a careful query returns strays. Check that the company
# is actually named in the headline or description.

in_title <- grepl(COMPANY_NAME, articles$title, ignore.case = TRUE)

desc_safe <- articles$description
desc_safe[is.na(desc_safe) == TRUE] <- ""
in_desc <- grepl(COMPANY_NAME, desc_safe, ignore.case = TRUE)

articles$is_relevant <- (in_title == TRUE | in_desc == TRUE)

relevant <- articles[articles$is_relevant == TRUE, ]

cat("---- Relevance filter ----\n")
cat("Before:", nrow(articles), "articles\n")
cat("After: ", nrow(relevant), "articles\n\n")

if (nrow(relevant) < nrow(articles)) {
  cat("Dropped headlines (company not named):\n")
  dropped <- articles[articles$is_relevant == FALSE, ]
  for (i in 1:min(nrow(dropped), 5)) {
    cat("  -", substr(dropped$title[i], 1, 75), "\n")
  }
  cat("\n")
}

# TIP: read those dropped headlines. Sometimes the filter is
# right and it caught junk. Sometimes it is wrong and the
# article was about the company under a different name (its
# CEO, a product, a subsidiary). No automatic filter is
# perfect, which is why you look at what it removed.

# ============================================================
# TURN IT INTO CONTEXT
# ============================================================

buildNewsContext <- function(article_rows, company) {
  if (nrow(article_rows) == 0) {
    return(paste0("No recent news found for ", company, "."))
  }

  lines <- character(0)
  n_show <- min(nrow(article_rows), 10)

  for (i in 1:n_show) {
    date_txt <- substr(article_rows$publishedAt[i], 1, 10)

    if ("source.name" %in% names(article_rows) == TRUE) {
      src_txt <- article_rows$source.name[i]
    } else {
      src_txt <- "Unknown source"
    }

    title_txt <- article_rows$title[i]

    desc_txt <- article_rows$description[i]
    if (is.na(desc_txt) == TRUE) {
      desc_txt <- ""
    } else {
      desc_txt <- paste0(" ", desc_txt)
    }

    lines <- c(lines, paste0("- [", date_txt, ", ", src_txt, "] ", title_txt, desc_txt))
  }

  block <- paste0("RECENT NEWS FOR ", toupper(company), "\n", paste(lines, collapse = "\n"))
  return(block)
}

news_context <- buildNewsContext(relevant, COMPANY_NAME)

cat("---- The context block ----\n")
cat(substr(news_context, 1, 1200), "\n")
if (nchar(news_context) > 1200) {
  cat("... (truncated for display)\n")
}
cat("\n")

cat("Size of this block:", nchar(news_context), "characters\n")
cat("Roughly", round(nchar(news_context) / 4), "tokens\n\n")

# TIP: we included the source and the date on every line. The
# model has no way to know whether Reuters or an unknown blog
# is more credible unless you tell it who published each item,
# and no sense of what is recent unless you date it. Context
# is not just the facts, it is the provenance of the facts.

# ============================================================
# SAVE IT
# ============================================================

pth <- file.path(savePth, "news_context_headlines.rds")
saveRDS(news_context, pth)
cat(paste0("Saved news_context_headlines.rds (the headline-only version) in\n"),
    pth)

# ============================================================
# ENRICHMENT: FILL IN WHAT NEWSAPI WOULD NOT GIVE US
# ============================================================

# We have breadth (many sources, dated, attributed) but no
# depth (200 characters of body text). So we take the headlines
# we just gathered and hand them to a model that CAN search
# the web and read what it finds.
#
# This is the pattern worth learning: use one API to work out
# what to ask, then use a second to answer it properly.
# NewsAPI is the better tool for "what happened and who
# reported it," because it is structured and cheap. A search
# model is the better tool for "what does it mean," because it
# can read the actual articles and synthesize them.
#
# Note what the model is and is not doing. It is reading and
# summarizing, which is a language task. It is not being asked
# to invent facts or compute anything. And because it returns
# citations, every claim can be traced back to a source.

if (DO_ENRICHMENT == TRUE) {

  openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

  if (nchar(openrouter_key) == 0) {
    cat("No OpenRouter key found, so the enrichment step is skipped.\n")
    cat("Set it with Sys.setenv(OPENROUTER_API_KEY = \"your_key\") and re-run.\n")
    enriched_context <- news_context
  } else {

    # ---- Build the query FROM the headlines ----
    # This is the hinge of the whole script. We are not asking
    # a vague question. We are telling the search model exactly
    # which stories we already know exist, so it goes and reads
    # those rather than wandering.

    headline_list <- character(0)
    n_seed <- min(nrow(relevant), 6)

    if (n_seed > 0) {
      for (i in 1:n_seed) {
        headline_list <- c(headline_list, paste0("- ", relevant$title[i]))
      }
    }
    headline_block <- paste(headline_list, collapse = "\n")

    user_prompt <- paste0(
      "I am researching ", COMPANY_NAME, " (", SYMBOL, ") as an investment.\n\n",
      "A news API returned these recent headlines:\n",
      headline_block, "\n\n",
      "Search for and read the underlying coverage, then give me a briefing that covers:\n",
      "1. What actually happened in each significant story, in a sentence or two.\n",
      "2. Any financial figures reported (revenue, margins, guidance, analyst targets).\n",
      "3. What is disputed or uncertain, where sources disagree.\n\n",
      "Be concise and factual. Do not speculate beyond what the sources say. ",
      "If something is unclear or unreported, say so."
    )

    system_prompt <- paste0(
      "You are a research assistant for an investment analyst. ",
      "You summarize what published sources report. You do not give investment advice, ",
      "and you do not state anything you cannot attribute to a source."
    )

    cat("Sending", n_seed, "headlines to", ENRICH_MODEL, "for enrichment.\n")
    cat("This one takes a few seconds, because the model is searching the web.\n")

    request_body <- list(
      model = ENRICH_MODEL,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      )
    )

    enrich_response <- POST(
      OPENROUTER_URL,
      add_headers(
        "Authorization" = paste("Bearer", openrouter_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(request_body, auto_unbox = TRUE),
      timeout(120)
    )

    enrich_code <- status_code(enrich_response)

    if (enrich_code != 200) {
      err_txt <- content(enrich_response, as = "text", encoding = "UTF-8")
      cat("\nEnrichment failed with HTTP status", enrich_code, "\n")
      cat("The API said:\n", substr(err_txt, 1, 400), "\n\n")
      cat("Continuing with the headline-only context.\n")
      enriched_context <- news_context
    } else {

      enrich_parsed <- fromJSON(content(enrich_response, as = "text", encoding = "UTF-8"),
                                flatten = TRUE)

      briefing <- enrich_parsed$choices$message.content[1]

      cat("Enrichment succeeded.\n\n")
      cat("---- The briefing ----\n")
      cat(briefing, "\n\n")

      # ---- Find the citations ----
      # Different models return sources in different places, so
      # rather than assume, we look and report what we found.
      # This is the same habit as printing field names first.
      citations <- NULL

      if (is.null(enrich_parsed$citations) == FALSE) {
        citations <- unlist(enrich_parsed$citations)
      } else if (is.null(enrich_parsed$choices$message.annotations) == FALSE) {
        ann <- enrich_parsed$choices$message.annotations[[1]]
        if (is.data.frame(ann) == TRUE && "url_citation.url" %in% names(ann) == TRUE) {
          citations <- ann$url_citation.url
        }
      }

      if (is.null(citations) == TRUE) {
        cat("No citation field was found in the response. Its structure is:\n")
        str(enrich_parsed, max.level = 2)
        cat("\n")
        citation_block <- ""
      } else {
        cat("---- Sources cited (", length(citations), ") ----\n", sep = "")
        for (i in 1:length(citations)) {
          cat("  [", i, "] ", citations[i], "\n", sep = "")
        }
        cat("\n")
        citation_block <- paste0("\n\nSOURCES\n",
                                 paste0("[", 1:length(citations), "] ", citations, collapse = "\n"))
      }

      # TIP: this is why a search model beats asking a plain
      # model "what is happening with Apple?" A plain model
      # answers from training data that has a cutoff date, and
      # gives you no way to check it. This one read current
      # pages and told you which ones. You can go verify.

      # ---- Assemble the fuller context ----
      enriched_context <- paste0(
        news_context, "\n\n",
        "BACKGROUND BRIEFING (compiled from web sources)\n",
        briefing,
        citation_block
      )
    }
  }

} else {
  cat("DO_ENRICHMENT is FALSE, so only headlines were collected.\n")
  enriched_context <- news_context
}

# ============================================================
# COMPARE THE TWO VERSIONS
# ============================================================

cat("---- Headlines only vs enriched ----\n")
cat("Headlines only:", nchar(news_context), "characters (",
    round(nchar(news_context) / 4), "tokens )\n")
cat("Enriched:      ", nchar(enriched_context), "characters (",
    round(nchar(enriched_context) / 4), "tokens )\n\n")

# TIP: the enriched version is bigger, and bigger is not
# automatically better. You spent tokens to buy depth. Whether
# that trade was worth it depends on the question you are
# about to ask. For "did anything major happen this week?"
# headlines were enough. For "should this change my thesis?"
# you needed the detail.
pth <- file.path(savePth, "news_context.rds")
saveRDS(enriched_context, pth)
cat("Saved news_context.rds (the enriched version) for use in FIN_D.\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Summary ----\n")
cat("1. Keys live in the environment, never in the script.\n")
cat("2. A vague query returns vague results. Be specific on purpose.\n")
cat("3. Know what your plan actually returns. Here, content stops at 200 characters.\n")
cat("4. Filter for relevance, then look at what the filter threw away.\n")
cat("5. Give the model the source and the date, not just the claim.\n")
cat("6. Use one API to decide what to ask, and another to answer it well.\n")
cat("7. Prefer a model that cites its sources, so its claims can be checked.\n")
