# ============================================================
# FIN_D_build_context.R
# LLM Context lesson, Day 2, Finance Masters track
# (8:00 to 9:00)
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
# work with, and seeing that difference is part of the point.
#
# This is where the hour lands. Everything before this was
# gathering. This is the decision: what earns a place in the
# window, and what gets left out.
# ============================================================

# ---- Libraries ----
library(httr)
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

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

archive_path <- path.expand(ARCHIVE_DIR)
all_csv <- list.files(archive_path, pattern = "\\.csv$", full.names = TRUE)

if (length(all_csv) == 0) {
  stop("No CSV files found in the archive folder.")
}

cat("Archive holds", length(all_csv), "transcript files.\n")

# File names follow SYMBOL_YYYY-MM-DD.csv, so everything we
# need to FIND the right call is in the name. We never open a
# file we are not going to use.
#
# Matching is a plain string comparison rather than a regular
# expression. Tickers like BRK.B contain characters that regex
# treats as wildcards, and a prefix test sidesteps that. It
# also enforces the underscore, so "A" cannot match "AAPL_..."
# and "ALL" cannot match "ALLY_...".
symbol_prefix <- paste0(SYMBOL, "_")
name_starts <- substr(basename(all_csv), 1, nchar(symbol_prefix))
symbol_files <- all_csv[name_starts == symbol_prefix]

if (length(symbol_files) == 0) {
  stop(paste("No files found for", SYMBOL, "in", archive_path))
}

# The date sits between the underscore and the .csv, so we can
# list every available call without reading anything.
file_dates <- substring(basename(symbol_files), nchar(symbol_prefix) + 1)
file_dates <- sub("\\.csv$", "", file_dates)

cat("Calls on file for", SYMBOL, ":",
    paste(sort(file_dates, decreasing = TRUE), collapse = ", "), "\n")

# ISO dates sort correctly as plain text, so the newest is
# simply the last one alphabetically.
newest_idx <- order(file_dates, decreasing = TRUE)[1]
chosen_file <- symbol_files[newest_idx]
latest_date <- file_dates[newest_idx]

this_call <- read.csv(chosen_file, stringsAsFactors = FALSE)

cat("Read 1 file of", length(all_csv), "in the archive:", basename(chosen_file), "\n")

if (nrow(this_call) == 0) {
  stop(paste("The file for", SYMBOL, "is empty."))
}

cat("Loaded", nrow(this_call), "speaker turns from the", latest_date, "call.\n")

# ---- Select, do not send everything ----
# FIN_A showed the whole call is far too large. So we choose.
# Here we take management remarks only, and only the first
# TRANSCRIPT_TURNS of them, which is roughly the prepared
# statement before Q&A begins.

isAnalyst <- function(title_text) {
  return(grepl("analyst|research", tolower(title_text)) == TRUE)
}

this_call$role_group <- ifelse(isAnalyst(this_call$title) == TRUE, "Analyst", "Company")

company_turns <- this_call[this_call$role_group == "Company", ]
n_take <- min(TRANSCRIPT_TURNS, nrow(company_turns))
excerpt_rows <- company_turns[1:n_take, ]

transcript_lines <- character(0)
for (i in 1:nrow(excerpt_rows)) {
  transcript_lines <- c(transcript_lines,
                        paste0(excerpt_rows$speaker[i], " (", excerpt_rows$title[i], "): ",
                               excerpt_rows$msg[i]))
}

transcript_context <- paste0(
  "EARNINGS CALL EXCERPT: ", SYMBOL, ", reported ", latest_date, "\n",
  "(management remarks only, first ", n_take, " turns)\n\n",
  paste(transcript_lines, collapse = "\n\n")
)

cat("Transcript excerpt:", nchar(transcript_context), "characters\n")
cat("That is", n_take, "of", nrow(company_turns), "management turns, and",
    nrow(this_call), "total turns exist.\n\n")

# TIP: we just discarded every analyst question. If the model
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
# two answers should probably match, and often they do not.

# ============================================================
# THE SYSTEM PROMPT
# ============================================================

# The context is the material. The system prompt is the
# instruction for how to treat it. Notice what this one does:
# it assigns a role, it separates fact from inference, it
# forbids fabrication, and it requires the model to say when
# something is missing. That last instruction is the one most
# people leave out, and it is the one that prevents the model
# from smoothing over a gap with a confident guess.

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
# ONE CALL
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

cat("---- Try this ----\n")
cat("1. Set TRANSCRIPT_TURNS to 3, re-run, and read the answer again.\n")
cat("   Less context is cheaper. Is the note meaningfully worse?\n")
cat("2. Include analyst turns as well as management, and see whether\n")
cat("   the model's read of the call changes.\n")
cat("3. Delete the line in the system prompt that says to admit gaps,\n")
cat("   re-run, and see whether the model starts filling them in.\n\n")

# That third experiment is the important one. A model asked a
# question it cannot answer from the context will usually
# answer anyway. The instruction not to is doing real work.

saveRDS(assembled_context, file.path(savePth, "assembled_context.rds"))
cat("Saved assembled_context.rds\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Summary ----\n")
cat("1. Context is assembled deliberately, from separate sources, with labels.\n")
cat("2. What you leave out shapes the answer as much as what you include.\n")
cat("3. Order matters: primary source first, question last.\n")
cat("4. The system prompt governs how the material is treated.\n")
cat("5. Telling a model to admit gaps is what stops it from inventing BUT does not always work.  Models are trained to be helpful and may hallucinate a fact and present it confidently.\n")
