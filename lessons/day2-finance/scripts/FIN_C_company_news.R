# ============================================================
# FIN_C_company_news.R
# LLM Context lesson, Day 2, Finance Masters track
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
#   usethis::edit_r_environ() 
#   OR
#   Sys.setenv(NEWSORG_API_KEY = "your_key_here")
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

# ============================================================
# CONFIGURATION
# ============================================================

NEWS_URL <- "https://newsapi.org/v2/everything"

# Save Path
# Create a folder in your repo called `context_files`
savePth <- '~/Desktop/vienna-genai-finance-course/context_files'

# The company to research.
SYMBOL       <- "AAPL"
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

# perplexity/sonar is cheap (about $1 per million tokens, with
# search included) and returns citations, which is why we use it
ENRICH_MODEL <- "perplexity/sonar"

# ============================================================
# BUILDING A GOOD QUERY
# ============================================================

# A search API answers exactly what you ask
query <- paste0("\"", 
                COMPANY_NAME, 
                "\" AND (earnings OR revenue OR stock OR shares OR analyst)")

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

# Did we get 200?
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

# So it succeeded, now let's extract our response
raw_text <- content(response, as = "text", encoding = "UTF-8")
parsed   <- fromJSON(raw_text, flatten = TRUE)

# The response wraps the results: status, totalResults, and
# then the articles table.
cat("Status field:", parsed$status, "\n")
cat("Total results available:", parsed$totalResults, "\n")
cat("Total returned in this call:", nrow(parsed$articles))

articles <- parsed$articles

# ============================================================
# WHAT DID WE GET?
# ============================================================

cat("---- Fields available ----\n")
names(articles)

cat("---- Headlines ----\n")
show_cols <- intersect(c("publishedAt", "source.name", "title"), names(articles))
preview <- articles[, show_cols]
print(head(preview, 3), row.names = FALSE)

# ============================================================
# THE LIMITATION YOU MUST KNOW ABOUT
# ============================================================

# The free plan does NOT give you article text. The `content`
# field is cut off at 200 characters. 
articles$content[1]
articles$content[2]

# So what you actually have per article is: a headline, a
# one-line description, and a 200 character stub. That is
# enough to know WHAT happened. It is not enough to do deep
# analysis of HOW it was reported.

# ============================================================
# TURN IT INTO CONTEXT
# ============================================================

buildNewsContext <- function(df, company){
  if(nrow(df)==0){
    block <- paste0("No recent news found for ", company, ".")
  } else {
    allArticle <- list()
    for(i in 1:nrow(df)){
      oneArticle <- df[i,]
      x <- paste0('Source Name: ', oneArticle$source.name,'\n',
                  'Published At: ', oneArticle$publishedAt,'\n',
                  'Article Title: ', oneArticle$title,'\n',
                  'Article Description: ', oneArticle$description, '\n', collapse = '\n')
      allArticle[[i]] <- x
    }
    block <- paste0(unlist(allArticle), collapse = '\n\n')
  }
  
  return(block)
}

news_context <- buildNewsContext(articles, COMPANY_NAME)

cat("Size of this block:", nchar(news_context), "characters\n")
cat("Roughly", round(nchar(news_context) / 4), "tokens\n\n")
cat(news_context)

# ============================================================
# SAVE IT
# ============================================================

pth <- file.path(savePth, "news_context_headlines.rds")
saveRDS(news_context, pth) #WILL OVERWRITE OLD NEWS CONTEXT
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
openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")


# ---- Build the query FROM the headlines ----
# This is the hinge of the whole script. We are not asking
# a vague question. We are telling the search model exactly
# which stories we already know exist, so it goes and reads
# those rather than wandering.
user_prompt <- paste0(
      "I am researching ", COMPANY_NAME, " (", SYMBOL, ") as an investment.\n\n",
      "A news API returned these recent headlines:\n",
      articles$source.name, "\n",
      articles$publishedAt, "\n",
      articles$title, "\n\n",
      "Search for and read the underlying news coverage, then give me a briefing that covers:\n",
      "1. What actually happened in each significant story, in a sentence or two.\n",
      "2. Any financial figures reported (revenue, margins, guidance, analyst targets).\n",
      "3. What is disputed or uncertain, where sources disagree.\n\n",
      "Be concise and factual. Do not speculate beyond what the sources say. ",
      "If something is unclear or unreported, say so."
    )

# Look at one user prompt
cat(tail(user_prompt,1))

# Some basic instructions
system_prompt <- paste0(
  "You are a research assistant for an investment analyst. ",
  "You summarize what published sources report. You do not give investment advice, ",
  "and you do not state anything you cannot attribute to a source.")

allOnlineResearch <- list()
for(i in 1:length(user_prompt)){
  print(paste('working on article', i, 'of', length(user_prompt)))
  
  # Set up
  request_body <- list(
    model = ENRICH_MODEL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt[i])
    )
  )
  
  # Post request
  enrich_response <- POST(
    OPENROUTER_URL,
    add_headers(
      "Authorization" = paste("Bearer", openrouter_key),
      "Content-Type" = "application/json"
    ),
    body = toJSON(request_body, auto_unbox = TRUE),
    timeout(60)
  )
  
  # Status check
  if(status_code(enrich_response)==200){
    cat("Enrichment succeeded.\n\n")
    
    # Parse the message response from the LLM
    message_content <- content(enrich_response)$choices[[1]]$message$content
    message_content <- paste('BACKGROUND INFORMATION COMPILED FROM THE WEB: ', 
                             message_content, '\n\n')
    
    # Flatten the annotations data
    annotationsURLS <- content(enrich_response)$choices[[1]]$message$annotations
    annotationsURLS <- do.call(rbind, lapply(annotationsURLS, function(ann) {
      if (ann$type == "url_citation") {
        data.frame(url   = ann$url_citation$url, stringsAsFactors = FALSE)}}))
    annotationsURLS <- paste('SOURCES: \n\n',
                             unlist(annotationsURLS), collapse = ' \n')
    
    briefing <- paste(message_content, annotationsURLS, collapse ='\n\n')
    
  } else { 
    cat("Enrichment failed\n\n")
    # Failed API so just put in title
    briefing <- articles$title[i]
  }
  
  enriched_context <- paste0(
    news_context, "\n\n",
    "BACKGROUND BRIEFING (compiled from web sources)\n",
    briefing)
}

# ============================================================
# Save the news with informational depth
# ============================================================

pth <- file.path(savePth, "news_context.rds")
saveRDS(enriched_context, pth)
cat("Saved news_context.rds (the enriched version) for use in FIN_D.\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Learning Review ----\n")
cat("1. Keys live in the environment, never in the script.\n")
cat("2. A vague query returns vague results. Be specific on purpose.\n")
cat("3. Know what your plan actually returns. Here, newsapi content stops at 200 characters.\n")
cat("4. Chaining APIs is a common practice.\n")
cat("5. Use one API to decide what to ask, and another to answer it well.\n")
cat("6. Prefer a model that cites its sources, so its claims can be checked.\n")
