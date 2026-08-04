# ============================================================
# FIN_G_human_review.R
# Human review surface, Day 2, Finance Masters track
# Bridge from R analysis to the vibe coded SPA (Hour 3)
#
# Purpose: assemble everything the morning produced into ONE
# artifact a human can review before it turns into a research
# note. Then hand that artifact, plus a prompt, off to a
# chatbot for the plain English write up.
#
# ============================================================
# THE HUMAN REVIEW SURFACE
# ============================================================
#
# Every script this morning produced a piece of a bigger
# picture. FIN_A showed the raw transcript. FIN_B pulled macro
# alerts. FIN_C pulled press coverage. FIN_D assembled those
# three into a context block and got the first research note.
# FIN_E scored sentiment turn by turn. FIN_F extracted
# entities, figures, and guidance.
#
# None of those on their own is a review surface. THIS script
# builds the review surface: one JSON file that gathers every
# piece of evidence and every derived label into one place, so
# a human can open it, read it, and decide whether it should
# become a research note that gets shared.
#
# This phrase is worth remembering. The human review surface
# is the checkpoint that Knight Capital skipped. Knight had
# code that acted the moment it fired. There was no surface,
# no artifact, no human between the model and the market. When
# you assemble the review surface deliberately and read it
# before publishing, you are doing the thing Knight did not.
#
# ============================================================
# WHAT THIS SCRIPT DOES AND DOES NOT DO
# ============================================================
#
# It does: read the outputs of FIN_D, FIN_E, and FIN_F, and
#          combine them into one JSON file and one companion
#          prompt file that a student pastes into any chatbot
#          (Gemini, Claude, ChatGPT, duck.ai) to get the final
#          research note.
#
# It does NOT: call an LLM. Zero API calls in this script.
#              Every LLM call happens in the chatbot the
#              you choose, outside R. That is the point.
#              The interpretation step is portable. The review
#              surface is not tied to any one model.
#
# Your SPA:
# In Hour 3 you rebuild everything this morning did in
# JavaScript, inside a single page application. Same shape,
# same split: code collects and structures the evidence, an
# LLM interprets the finished evidence. The JSON here
# is the object your SPA will build & produce a note. 
# ============================================================

# ---- Libraries ----
# Only jsonlite. No API calls means no httr, no httr2.
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

# Shared output folder, relative to the RStudio project root.
savePth <- "context_files"

# For labeling. FIN_G does not choose a symbol, it reads what
# the upstream scripts produced. The symbol below is used only
# in the file name and in the printed prompt.
SYMBOL <- "AAPL"
REPORTING_COMPANY <- "Apple"

# Where to write the two output files.
JSON_OUT <- file.path(savePth, paste0("human_review_surface_", SYMBOL, ".json"))
PROMPT_OUT <- file.path(savePth, paste0("chatbot_prompt_", SYMBOL, ".txt"))

# ============================================================
# LOAD WHAT THE MORNING PRODUCED
# ============================================================

# Every upstream script (B, C, D, E, F) writes into savePth.
# We read from the same place. One convention, one folder,
# no fallback logic.

loadOrDie <- function(file_name, source_script) {
  full_path <- file.path(savePth, file_name)
  if (file.exists(full_path) == FALSE) {
    stop(paste0("Missing: ", full_path,
                "\nRun ", source_script, " first."))
  }
  return(readRDS(full_path))
}

assembled_context <- loadOrDie("assembled_context.rds", "FIN_D")
cat("Loaded assembled context.\n")

sentiment_df <- loadOrDie("sentiment_scores.rds", "FIN_E")
sentiment_summary <- loadOrDie("sentiment_summary.rds", "FIN_E")
cat("Loaded sentiment data:", nrow(sentiment_df), "scored turns.\n")

extraction_df <- loadOrDie("extraction_per_turn.rds", "FIN_F")
extraction_summary <- loadOrDie("extraction_summary.rds", "FIN_F")
cat("Loaded extraction data:", nrow(extraction_df), "extracted turns.\n\n")

# ============================================================
# SANITY CHECK
# ============================================================

# The upstream scripts should have used the same symbol
# and report date. If not, something is stale. Flag it.

sent_symbol <- sentiment_summary$symbol
sent_date <- sentiment_summary$report_date
extr_symbol <- extraction_summary$symbol
extr_date <- extraction_summary$report_date

cat("---- Coverage check ----\n")
cat("Sentiment covers:", sent_symbol, "on", sent_date, "\n")
cat("Extraction covers:", extr_symbol, "on", extr_date, "\n\n")

if (sent_symbol != extr_symbol || sent_date != extr_date) {
  cat("WARNING: sentiment and extraction do not cover the same call.\n")
  cat("One of them was run against a different symbol or a different date.\n")
  cat("Re-run FIN_E and FIN_F with matching settings before trusting this output.\n\n")
}

# ============================================================
# ASSEMBLE THE REVIEW SURFACE
# ============================================================

# One nested list, which jsonlite will turn into pretty
# printed JSON. The structure mirrors the morning: sources,
# then measurements, then extractions.

review_surface <- list(

  meta = list(
    symbol = sent_symbol,
    reporting_company = REPORTING_COMPANY,
    report_date = sent_date,
    assembled_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  ),

  # The prose context from FIN_D: transcript excerpt, news
  # briefing, macro alerts, in one string with section
  # headers. This is the raw material.
  context = list(
    assembled_text = assembled_context,
    length_chars = nchar(assembled_context)
  ),

  # Sentiment reduced to just what matters for a research note.
  # We include the roll ups (overall, by role) and a short list
  # of the most positive and most negative speakers. The full
  # per turn scores stay in sentiment_scores.rds for audit,
  # they do not need to be in the review surface.
  sentiment = list(
    overall = sentiment_summary$overall,
    by_role = sentiment_summary$by_role,
    most_positive_speakers = head(
      sentiment_summary$by_speaker[order(sentiment_summary$by_speaker$sentiment_density,
                                        decreasing = TRUE), ],
      3
    ),
    most_negative_speakers = head(
      sentiment_summary$by_speaker[order(sentiment_summary$by_speaker$sentiment_density,
                                        decreasing = FALSE), ],
      3
    )
  ),

  # Extraction: everything from FIN_F's aggregate. Entities are
  # deduped, claims (figures, forward looking) are attributed
  # to the speaker.
  extraction = list(
    companies_mentioned = extraction_summary$unique_companies,
    executives = extraction_summary$unique_executives,
    products_and_segments = extraction_summary$unique_products,
    financial_figures = extraction_summary$financial_figures,
    forward_looking_statements = extraction_summary$forward_looking_statements
  )
)

# ============================================================
# WRITE THE JSON
# ============================================================

if (dir.exists(path.expand(savePth)) == FALSE) {
  dir.create(path.expand(savePth), recursive = TRUE)
}

# pretty = TRUE so the JSON is human readable. auto_unbox
# converts length one vectors to scalars, which is what a
# human expects (a symbol should read as "AAPL", not ["AAPL"]).
review_json <- toJSON(review_surface,
                      pretty = TRUE,
                      auto_unbox = TRUE,
                      na = "null")

writeLines(review_json, path.expand(JSON_OUT))

cat("---- Human review surface written ----\n")
cat("File: ", JSON_OUT, "\n")
cat("Size: ", format(nchar(review_json), big.mark = ","), "characters\n")
cat("     roughly", round(nchar(review_json) / 4), "tokens\n\n")

# TIP: open that file before you do anything else. Read it.
# The whole point of building a review surface is that a
# human reads it before it becomes a note. If it looks wrong
# here, it is going to look wrong in the research note too,
# and there is no way to find that out except by looking.

# ============================================================
# BUILD THE CHATBOT PROMPT
# ============================================================

# Two prompts: a system prompt that sets the rules, and a
# user prompt that asks the question. Both are written to a
# file so we can have examples to put into any chatbot we want. 
#
# The prompt is written to be model neutral. It works in
# Gemini, in Claude, in ChatGPT, in duck.ai, etc. The rules are
# the same rules FIN_D used, extended for the sentiment and
# extraction we now have.

chatbot_system_prompt <- paste0(
  "You are an equity research assistant preparing notes for an analyst.\n",
  "You will be given a JSON packet containing an earnings call context, sentiment measurements, and extracted facts for ",
  REPORTING_COMPANY, " (", SYMBOL, ").\n\n",
  "Rules:\n",
  "1. Use only the provided packet. Do not add outside knowledge or search the web.\n",
  "2. Attribute every claim to which section it came from (context, sentiment, or extraction).\n",
  "3. Distinguish what management asserted from what analysts questioned.\n",
  "4. Quote forward looking statements verbatim when you cite them.\n",
  "5. If the packet does not answer part of the question, say so plainly.\n",
  "6. Do not give investment advice or a buy/sell view.\n",
  "7. Be concise. Bullet points and short paragraphs, not walls of prose."
)

chatbot_user_prompt <- paste0(
  "Attached below is a research packet for ", REPORTING_COMPANY, " (", SYMBOL, ").\n",
  "Please draft a one page research note covering:\n\n",
  "1. What management emphasized in this quarter. Which products, which segments, which figures?\n",
  "2. How the tone reads. Is management more positive than the analysts? Which speaker was most positive, which most negative?\n",
  "3. The specific forward looking statements management made. Quote them.\n",
  "4. Where the context (news, macro alerts) supports or complicates the company's own account.\n",
  "5. What you would want to see next quarter to confirm or reject this reading.\n\n",
  "Attach or paste the JSON packet after this message.\n"
)

# ---- Assemble the copy paste block ----

full_prompt_block <- paste0(
  "============================================================\n",
  "SYSTEM PROMPT (paste as the system message)\n",
  "============================================================\n\n",
  chatbot_system_prompt, "\n\n",
  "============================================================\n",
  "USER PROMPT (paste as the user message, followed by the JSON)\n",
  "============================================================\n\n",
  chatbot_user_prompt, "\n",
  "============================================================\n",
  "JSON PACKET\n",
  "(if your chatbot does not accept file attachments, paste this JSON directly after the user prompt)\n",
  "============================================================\n\n",
  review_json, "\n"
)

writeLines(full_prompt_block, path.expand(PROMPT_OUT))

cat("---- Chatbot prompt written ----\n")
cat("File:", PROMPT_OUT, "\n")
cat("Contains: system prompt, user prompt, and the JSON packet.\n\n")

# ============================================================
# PRINT THE PROMPTS TO THE CONSOLE
# ============================================================

# Also print them here so students can see what they are
# going to paste, without opening the file.

cat("============================================================\n")
cat("READY TO HAND OFF TO A CHATBOT\n")
cat("============================================================\n\n")

cat("Open ", PROMPT_OUT, ",\n", sep = "")
cat("copy everything, and paste it into any chatbot\n")

cat("If your chatbot accepts file attachments, attach\n")
cat("  ", JSON_OUT, "\n", sep = "")
cat("and paste ONLY the system and user prompts above the attach point.\n\n")

cat("---- The system prompt ----\n")
cat(chatbot_system_prompt, "\n\n")

cat("---- The user prompt ----\n")
cat(chatbot_user_prompt, "\n")

cat('Example shared conversation here: https://share.gemini.google/1tt3nriDc9MI')

cat("============================================================\n")
cat("Now for your SPA Build\n")
cat("============================================================\n\n")

cat("R's manual, behind the scenes context build is done.  What you built in R, you now build in your vibe coded application.\n")
cat("What should NOT change: the split. Code collects and structures evidence.\n")
cat("An LLM interprets finished evidence, writing the research note. A human reads the review surface\n")


# End
