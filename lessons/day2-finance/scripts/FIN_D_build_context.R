# ============================================================
# FIN_D_build_context.R
# LLM Context lesson, Day 2, Finance Masters track
#
# Purpose: put the three pieces together. A transcript excerpt,
# a macro risk block, and a news briefing become ONE assembled
# context, sent with a system prompt, in a single call.
#
# How to use this script: run it a block at a time and read
# what prints.
#
# Run FIN_B and FIN_C first so their saved blocks exist. If
# they are missing this script still runs, it just has less to
# work with, and may be too generic to be useful.
#
# Everything before this was data gathering. 
# As a practitioner you decide the contents, this is the decision: 
# what earns a place in the window, and what gets left out.
# ============================================================

# ---- Libraries ----
library(httr)
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

# can be your local CSVs of earnings calls folder
ARCHIVE_DIR <- "~/Desktop/vienna-genai-finance-course/earnings_call_archive/transcripts_sp500_marketbeat"

savePth <- "~/Desktop/vienna-genai-finance-course/context_files"

SYMBOL <- "AAPL"
COMPANY_NAME <- "Apple"

OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"

# A plain model this time, with no web search. Everything it
# knows about this company comes from the context WE built.
# That is the experiment: the quality of the answer is now a
# direct measure of the quality of our context.
MODEL <- "google/gemini-3.5-flash-lite"

# How much of the transcript to include. This is the number
# you will want to change and re-run.
TRANSCRIPT_TURNS <- 12

CHARS_PER_TOKEN <- 4

# ============================================================
# THE API KEY
# ============================================================

openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

if (nchar(openrouter_key) == 0) {
  stop("No OpenRouter key. Run Sys.setenv(OPENROUTER_API_KEY = \"your_key\") in the console.")
} else {
  cat("OpenRouter key found.\n")
}

# ============================================================
# PIECE 1: THE TRANSCRIPT
# ============================================================

# We reload the CSV rather than reading a saved object from
# FIN_A. Local files are free to re-read, so there is no
# reason to cache them. API results are different, which is
# why FIN_B and FIN_C saved theirs.
symbol_files <- list.files(ARCHIVE_DIR,
                           pattern = paste0("^", SYMBOL, "_.*\\.csv$"),
                           full.names = TRUE)

# The date sits between the underscore and the .csv, so we can
# list every available call without reading anything.
file_dates <- substring(basename(symbol_files), nchar(SYMBOL) + 2)
file_dates <- sub("\\.csv$", "", file_dates)
cat("Calls on file for", SYMBOL, ":",
    paste(sort(file_dates, decreasing = TRUE), collapse = ", "), "\n")

# Find the latest
newest_idx  <- order(file_dates, decreasing = TRUE)[1]
chosen_file <- symbol_files[newest_idx]
latest_date <- file_dates[newest_idx]

# Load the latest and quick check
this_call <- read.csv(chosen_file, stringsAsFactors = FALSE)
head(this_call$msg,2)

# Let's just send the analyst comments and questions
isAnalyst <- grepl("analyst|research|managing director", 
                   this_call$title, 
                   ignore.case = T)
this_call$role_group <- ifelse(isAnalyst==T, "Analyst","Company")

# Subset to company statements
transcript_context <- subset(this_call, this_call$role_group == 'Company')
transcript_context <- paste(transcript_context$msg, collapse = '\n')

# Reminder: we just discarded every analyst question. If the model
# later says the call was positive, remember that we only
# showed it the part where management describes their own
# quarter. We built that bias in ourselves.

# ============================================================
# PIECE 2: MACRO RISK (from FIN_B)
# ============================================================
macro_context <- readRDS(file.path(savePth, "macro_context.rds"))
cat("Macro block loaded:", nchar(macro_context), "characters\n")


# ============================================================
# PIECE 3: NEWS (from FIN_C)
# ============================================================
news_context <- readRDS(file.path(savePth, "news_context.rds"))
cat("News block loaded:", nchar(news_context), "characters\n\n")

# ============================================================
# ASSEMBLE
# ============================================================

# Order matters more than people expect. Models attend
# unevenly across a long input, and the beginning and end get
# the most weight. We put the transcript first because it is
# the primary source, and the question last so it is the final
# thing read. Labels matter too: without headers the model
# cannot tell where a company's own claims end and a
# journalist's summary begins.

assembled_context <- paste0(
  "=== PRIMARY SOURCE: COMPANY STATEMENTS ===\n",
  transcript_context, "\n\n",
  "=== SECONDARY SOURCE: PRESS COVERAGE ===\n",
  news_context, "\n\n",
  "=== ENVIRONMENT: MACRO RISK ===\n",
  macro_context
)

cat("---- The assembled context ----\n")
cat("Transcript:", nchar(transcript_context), "characters\n")
cat("News:      ", nchar(news_context), "characters\n")
cat("Macro:     ", nchar(macro_context), "characters\n")
cat("TOTAL:     ", nchar(assembled_context), "characters, roughly",
    round(nchar(assembled_context) / CHARS_PER_TOKEN), "tokens\n\n")

# TIP: look at those proportions. Which source is taking up
# most of your window? Is that the one you most trust? Those
# two answers should probably match.

# ============================================================
# THE SYSTEM PROMPT
# ============================================================

# The context is the material we decided upon. The system prompt is the
# instruction for how to treat it. Notice what this one does:
# it assigns a role, it separates fact from inference, it
# forbids fabrication, and it requires the model to say when
# something is missing. That last instruction is the one most
# people leave out, and it is the one that prevents the model
# from smoothing over a gap with a confident guess...mostly!

system_prompt <- paste0(
  "You are an equity research assistant preparing notes for an analyst.\n",
  "You will be given three sources: a company's own earnings call remarks, ",
  "recent press coverage, and current macro risk alerts.\n\n",
  "Rules:\n",
  "1. Use only the provided sources. Do not add outside knowledge.\n",
  "2. Attribute every claim to which source it came from.\n",
  "3. Distinguish what the company asserts from what others report.\n",
  "4. If the sources do not answer part of the question, say so plainly.\n",
  "5. Do not give investment advice or a buy/sell view.\n",
  "6. Be concise."
)

user_question <- paste0(
  "For ", COMPANY_NAME, " (", SYMBOL, "):\n",
  "1. What did management emphasize in this call?\n",
  "2. Does press coverage support or complicate that picture?\n",
  "3. Do any macro alerts bear on this company specifically?\n",
  "4. What would you need that these sources do not contain?"
)

full_user_message <- paste0(assembled_context, "\n\n=== QUESTION ===\n", user_question)

cat("---- What we are sending ----\n")
cat("System prompt:", nchar(system_prompt), "characters\n")
cat("User message: ", nchar(full_user_message), "characters\n")
cat("Estimated total:", round((nchar(system_prompt) + nchar(full_user_message)) / CHARS_PER_TOKEN),
    "tokens\n\n")

# ============================================================
# ONE CALL WITH ALL THE CONTEXT ADDED
# ============================================================

cat("Sending to", MODEL, "\n")

request_body <- list(
  model = MODEL,
  messages = list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = full_user_message)
  )
)

response <- POST(
  OPENROUTER_URL,
  add_headers(
    "Authorization" = paste("Bearer", openrouter_key),
    "Content-Type" = "application/json"
  ),
  body = toJSON(request_body, auto_unbox = TRUE),
  timeout(120)
)

code <- status_code(response)

if (code != 200) {
  err_txt <- content(response, as = "text", encoding = "UTF-8")
  cat("\nRequest failed with HTTP status", code, "\n")
  cat("The API said:\n", substr(err_txt, 1, 400), "\n")
  stop("Stopping. See the message above.")
} else {
  cat("Response received.\n\n")
}

parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)

answer <- parsed$choices$message.content[1]

cat("---- The research note ----\n")
cat(answer, "\n\n")

# ============================================================
# WHAT DID IT COST?
# ============================================================

# The API reports actual token usage, which is more accurate
# than our character estimate. Compare the two.

if (is.null(parsed$usage) == FALSE) {
  cat("---- Actual usage ----\n")
  cat("Prompt tokens:    ", parsed$usage$prompt_tokens, "\n")
  cat("Completion tokens:", parsed$usage$completion_tokens, "\n")
  cat("Total tokens:     ", parsed$usage$total_tokens, "\n\n")

  cat("Our estimate was", round((nchar(system_prompt) + nchar(full_user_message)) / CHARS_PER_TOKEN),
      "prompt tokens.\n")
  cat("Dividing characters by", CHARS_PER_TOKEN, "is a rough rule, not a precise one.\n\n")
} else {
  cat("No usage field was returned.\n\n")
}

# ============================================================
# NOW CHANGE SOMETHING
# ============================================================

cat("---- TIME PERMITTING:\n\nTry these options to compare how context impacts quality ----\n")
cat('Save a copy of this script with save as')
cat("Less context is cheaper. \nIs the answer meaningfully worse as you try these changes?\n")
cat('- recreate transcript_context but limit it to the first 5 turns.')
cat(" - Include analyst turns as well as management")
cat(" - Delete the line in the system prompt that says to admit gaps,\n")

saveRDS(assembled_context, file.path(savePth, "assembled_context.rds"))
cat("Saved assembled_context.rds\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Learning Review ----\n")
cat("1. Context is assembled deliberately, from separate sources, with labels.\n")
cat("2. What you leave out shapes the answer as much as what you include.\n")
cat("3. Order matters: primary source first, question last.\n")
cat("4. The system prompt governs how the informational context is treated.\n")
cat("5. Telling a model to admit gaps may stop it from inventing BUT does not always work.  Models are trained to be helpful and may hallucinate a fact and present it confidently.\n")
