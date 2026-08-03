# ============================================================
# FIN_A_load_documents.R
# LLM Context lesson, Day 2, Finance Masters track
#
# Purpose: open a real earnings call transcript, see how big
# it actually is, and learn why "context" is something you
# build on purpose rather than something you just paste in.
#
# How to use this script: run it a block at a time and read
# what prints. The point is what you notice, not what you type.
#
# A NOTE ON THIS TRACK. In the Data Science track the rule is
# that the language model never does arithmetic. Here the rule
# flips: the model does the core language work, because reading text,
# judging tone, and pulling out names ARE language tasks. The model is being
# used for what it is actually good at.
# ============================================================

# ---- Libraries ----
# No API calls in this script at all. Everything is local.

# ============================================================
# CONFIGURATION
# ============================================================

# Folder holding the scraped transcript CSVs.
ARCHIVE_DIR <- "~/Desktop/vienna-genai-finance-course/earnings_call_archive/transcripts_sp500_marketbeat"

# Which company to look at. Change this and re-run.
SYMBOL <- "AAPL"

# A rough conversion for estimating tokens from characters.
# English text averages close to 4 characters per token. This
# is an approximation, not a billing-grade number, but it is
# accurate enough to reason about limits.
CHARS_PER_TOKEN <- 4

# A pretend context budget, so we can see what fits.
# Real models allow far more, but every model has a ceiling
# and every token costs money.
CONTEXT_BUDGET_TOKENS <- 8000

# ============================================================
# LOAD THE TRANSCRIPTS
# ============================================================

# The scraper writes CSV files with one row per speaker turn:
#   symbol, report_date, speaker, title, msg, source_url
# We read every CSV in the folder and stack them, so this
# works whether the scraper made one big file or one file
# per call.

archive_path <- path.expand(ARCHIVE_DIR)

if (dir.exists(archive_path) == FALSE) {
  stop(paste("Archive folder not found at:", archive_path))
}

csv_files <- list.files(archive_path, pattern = "\\.csv$", full.names = TRUE)

# The instructor manifest is not transcript data, so skip it.
csv_files <- csv_files[grepl("_manifest", basename(csv_files)) == FALSE]
csv_files <- csv_files[grepl("sp500_companies", basename(csv_files)) == FALSE]

if (length(csv_files) == 0) {
  stop("No transcript CSV files found in the archive folder.")
} else {
  cat("Found", length(csv_files), "CSV file(s) in the archive.\n")
}

all_rows <- list()
for (i in 1:length(csv_files)) {
  one <- read.csv(csv_files[i], stringsAsFactors = FALSE)
  all_rows[[i]] <- one
}
transcripts <- do.call(rbind, all_rows)

cat("Total speaker turns loaded:", nrow(transcripts), "\n")
cat("Columns available:", paste(names(transcripts), collapse = ", "), "\n\n")

# TIP: notice the shape of this data. A transcript is not one
# giant blob of text. It arrives as ROWS, one per speaker
# turn. That structure is going to be useful all morning.

# ============================================================
# PICK ONE CALL
# ============================================================

this_call <- transcripts[transcripts$symbol == SYMBOL, ]

if (nrow(this_call) == 0) {
  stop(paste("No transcript rows found for", SYMBOL, "- try a different symbol."))
}

# If the archive holds several quarters for this company,
# take the most recent one so we are looking at a single call.
available_dates <- sort(unique(this_call$report_date), decreasing = TRUE)
cat("Calls available for", SYMBOL, ":", paste(available_dates, collapse = ", "), "\n")

chosen_date <- available_dates[1]
this_call <- this_call[this_call$report_date == chosen_date, ]

cat("Using the call from:", chosen_date, "\n")
cat("Speaker turns in this call:", nrow(this_call), "\n\n")

# ============================================================
# WHO IS IN THE ROOM?
# ============================================================

# The `title` column tells us each speaker's role. Earnings
# calls have two very different halves: management presenting
# prepared remarks, and analysts asking questions.

speakers <- unique(this_call[, c("speaker", "title")])
cat("---- Participants ----\n")
print(speakers, row.names = FALSE)

# A simple rule to separate the two groups. It is not perfect,
# and looking at where it gets things wrong is worthwhile.
role_group <- grepl("analyst|research|managing director", 
                   this_call$title,
                   ignore.case = T)

this_call$role_group <- ifelse(role_group == TRUE, "Analyst", "Company")

cat("Turns by group:\n")
print(table(this_call$role_group))

# TIP: In FIN_F we score sentiment
# separately for each group. Management is almost always
# positive about their own quarter. The interesting question
# is whether the analysts agree.

# ============================================================
# HOW BIG IS THIS THING?
# ============================================================

this_call$msg_chars <- nchar(this_call$msg)

total_chars <- sum(this_call$msg_chars)
estimated_tokens <- round(total_chars / CHARS_PER_TOKEN)

cat("---- Size of one earnings call ----\n")
cat("Total characters:", format(total_chars, big.mark = ","), "\n")
cat("Estimated tokens:", format(estimated_tokens, big.mark = ","), "\n")
cat("Longest single turn:", format(max(this_call$msg_chars), big.mark = ","), "characters\n")
cat("Average turn:", format(round(mean(this_call$msg_chars)), big.mark = ","), "characters\n\n")

# STOP AND LOOK AT THAT NUMBER.
# This is the whole reason "context engineering" is a phrase.
# One call with all information is enormous. Now imagine you wanted to give the
# model four quarters of calls, plus recent news, plus a macro
# risk feed, and still leave room for its answer.

if (estimated_tokens > CONTEXT_BUDGET_TOKENS) {
  cat("This single call is roughly",
      round(estimated_tokens / CONTEXT_BUDGET_TOKENS, 1),
      "times our", format(CONTEXT_BUDGET_TOKENS, big.mark = ","),
      "fake token budget.  Although artificial it will help with model attention & costs to be specific.\n")
  cat("We cannot just paste the whole thing in. We have to choose.\n\n")
} else {
  cat("This call fits inside our token budget with room to spare.\n\n")
}

# ============================================================
# STRATEGY 1: SELECT, DO NOT SUMMARIZE
# ============================================================

# Because the data is
# already split by speaker, we can keep only what matters for
# the question we are asking.

company_only <- this_call[this_call$role_group == "Company", ]
company_chars <- sum(company_only$msg_chars)

cat("---- Strategy 1: keep only management remarks ----\n")
cat("Characters:", format(company_chars, big.mark = ","), "\n")
cat("Estimated tokens:", format(round(company_chars / CHARS_PER_TOKEN), big.mark = ","), "\n")
cat("Reduction:", round(100 * (1 - company_chars / total_chars)), "percent smaller\n\n")

# TIP: this is a real decision with a real cost. You just
# threw away every analyst question. If you are asking "what
# did management claim?", that is fine. If you are asking
# "did anyone push back?", you have deleted the answer.

# ============================================================
# STRATEGY 2: CHUNK THE DOCUMENT
# ============================================================

# When you do need all of it, you split the document into
# pieces that each fit, process them one at a time, then
# combine. The naive way is to cut every N characters, which
# slices sentences and speakers in half.
#
# We have something better. Speaker turns are natural
# boundaries, so we pack whole turns into chunks and never cut
# anyone off mid-sentence.

CHUNK_BUDGET_CHARS <- CONTEXT_BUDGET_TOKENS * CHARS_PER_TOKEN

chunk_id <- integer(nrow(this_call))
current_chunk <- 1
running_total <- 0

# walk through each turn in order and keep a running character count. 
# When adding the next turn would exceed CHUNK_BUDGET_CHARS, start a new chunk to send to the LLM
for (i in 1:nrow(this_call)) {
  turn_size <- this_call$msg_chars[i]
  if (running_total + turn_size > CHUNK_BUDGET_CHARS && running_total > 0) {
    current_chunk <- current_chunk + 1
    running_total <- 0
  }
  chunk_id[i] <- current_chunk
  running_total <- running_total + turn_size
}

this_call$chunk <- chunk_id

cat("---- Strategy 2: chunk on speaker boundaries ----\n")
cat("Chunks needed:", max(this_call$chunk), "\n")
cat("Turns per chunk:\n")
print(table(this_call$chunk))

# TIP: chunking costs you something too. A model reading
# chunk 3 has no memory of chunk 1. If an analyst refers back
# to something said forty minutes earlier, that connection is
# gone. Every context strategy trades away something. Knowing
# what you traded is the skill.

# ============================================================
# LOOK AT THE ACTUAL TEXT
# ============================================================

# Everything above is about size. Now read a little of it, so
# the numbers connect to something real.

cat("---- A management turn ----\n")
first_company <- company_only[1, ]
cat(first_company$speaker, " (", first_company$title, ")\n\n", sep = "")
cat(substr(first_company$msg, 1, 700), "\n\n")

analyst_only <- this_call[this_call$role_group == "Analyst", ]
if (nrow(analyst_only) > 0) {
  cat("---- An analyst turn ----\n")
  first_analyst <- analyst_only[1, ]
  cat(first_analyst$speaker, " (", first_analyst$title, ")\n\n", sep = "")
  cat(substr(first_analyst$msg, 1, 700), "\n\n")
} 

# TIP: read those two side by side. Management language is
# polished and prepared. Analyst language is direct and often
# skeptical. A sentiment model will score them very
# differently, and it should.

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("---- Learning Review ----\n")
cat("1. A single earnings call is far larger than a casual prompt.\n")
cat("2. Context is built by choosing, not by pasting everything.\n")
cat("3. Speaker structure gives you both a filter and a chunk boundary.\n")
cat("4. Every strategy discards something. Know what you discarded.\n")

# End
