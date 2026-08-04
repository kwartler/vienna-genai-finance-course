# ============================================================
# FIN_E_sentiment.R
# Sentiment lesson, Day 2, Finance Masters track
#
# Purpose: score sentiment across an entire earnings call by
# asking a language model to identify positive and negative
# phrases in every speaker turn. The result is a data frame
# you can slice by speaker, roll up to a role group, or
# reduce to one overall label for the call.
#
# How to use this script: run it a block at a time and read
# what prints. The first row is the interesting one. Watch
# what the model returns for that turn before the loop starts,
# then follow the loop.
#
# THREE THINGS THAT ARE NEW IN THIS SCRIPT.
#
# 1. httr2 with retry. Every call in this script is one of
#    fifty or more, and a single network hiccup should not
#    force a rerun of the whole loop. httr2's req_retry()
#    gives us backoff and retry in one line, which is what
#    you want any time you loop over an API.
#
# 2. JSON schema validation. Instead of asking the model to
#    "please return JSON" and cleaning up the result, we hand
#    it a schema. OpenRouter enforces the shape at the
#    provider layer, so the response arrives already parseable.
#    No fence stripping, no missing field surprises.
#
# 3. Open weight versus closed weight. The model we use here
#    is open weight, meaning its parameters are published under
#    a permissive license. Closed weight models like Gemini and
#    Claude keep their parameters private. Both call the same
#    way through OpenRouter. Open weight models cost less per
#    token, could run on your own hardware if you ever needed
#    to, and cannot be silently deprecated by a provider.
#    Closed weight models tend to be stronger at the frontier
#    of capability, but for a bounded task like sentiment
#    classification the gap has closed. This is a good place
#    to use open weight and pocket the savings.
#
# APPROACH: PER ROW, NOT BATCHED.
# We send one API call per speaker turn. That is slower and
# more expensive than putting the whole transcript into one
# request and asking for a JSON array back. We do it this way
# on purpose. Per row, you see the mechanic clearly on the
# first turn, and if any single call fails you inspect just
# that one. Batched is the right choice in production once
# you trust the shape. Ask about it if you want the pattern.
#
# APPROACH: BINARY LABELS, DENSITY WEIGHTED AGGREGATION.
# The model returns lists of positive and negative phrases
# per turn. We do not ask it for a numeric score. We derive
# the label ourselves from the counts and the length of the
# passage, so the arithmetic is visible and auditable. A CEO
# using ten positive phrases in a hundred words is more
# positive than the same ten positive phrases in a thousand
# words, and the density metric captures that.
# ============================================================

# ---- Libraries ----
# httr2:    the modern successor to httr, with built in retry
# jsonlite: JSON parsing (still the right tool for this)
library(httr2)
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

ARCHIVE_DIR <- file.path("earnings_call_archive", "transcripts_sp500_marketbeat")

savePth <- "context_files"

# The company to score. Change and re-run.
SYMBOL <- "AAPL"

OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Open weight, cheap, reliable at JSON schema. Kept as a top
# level constant so switching to a closed model (for example
# google/gemini-3.5-flash-lite) is a one line change.
MODEL <- "mistralai/mistral-small-3.2-24b-instruct"

# Rows shorter than this are dropped before scoring. A twenty
# character "thank you" turn cannot carry sentiment and would
# only burn an API call. Raise this if you want to focus on
# substantive remarks only.
MIN_ROW_CHARS <- 40

# httr2 retry policy. Three tries covers most transient
# failures without turning a real error into a long wait.
RETRY_MAX <- 3

# The density threshold that separates "leaning positive" or
# "leaning negative" from "neutral" during aggregation. In
# plain terms: (positive_phrases minus negative_phrases)
# divided by total words. A value of 0.005 means the passage
# needs at least half a percentage point more of one than the
# other before we call it pos or neg. Adjust and see how the labels
# shift.
NEUTRAL_BAND <- 0.005

# ============================================================
# THE API KEY
# ============================================================

openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

# ============================================================
# LOAD ONE CALL (same pattern as FIN_D)
# ============================================================

symbol_files <- list.files(ARCHIVE_DIR,
                           pattern = paste0("^", SYMBOL, "_.*\\.csv$"),
                           full.names = TRUE)

# Find the latest
file_dates <- substring(basename(symbol_files), nchar(SYMBOL) + 2)
file_dates <- sub("\\.csv$", "", file_dates)

# Load the latest and quick check
newest_idx  <- order(file_dates, decreasing = TRUE)[1]
chosen_file <- symbol_files[newest_idx]
latest_date <- file_dates[newest_idx]
this_call <- read.csv(chosen_file, stringsAsFactors = FALSE)

# ============================================================
# TAG ROLES, DROP SHORT TURNS
# ============================================================

# Assign a speaking group
isAnalyst <- grepl("analyst|research|managing director", 
                   this_call$title, 
                   ignore.case = T)
this_call$role_group <- ifelse(isAnalyst==T, "Analyst","Company")
this_call$msg_chars <- nchar(this_call$msg)

kept <- this_call[this_call$msg_chars >= MIN_ROW_CHARS, ]

cat("---- Filtering short turns ----\n")
cat("Turns loaded:      ", nrow(this_call), "\n")
cat("Turns kept (>= ", MIN_ROW_CHARS, " chars): ", nrow(kept), "\n", sep = "")

# Preserve the original turn index so it lines up with FIN_A.
kept$turn_index <- as.integer(rownames(kept))
rownames(kept) <- NULL

# ============================================================
# THE JSON SCHEMA
# ============================================================

# This is what OpenRouter passes to the provider. `strict`
# tells the provider to enforce the shape.

sentiment_schema <- list(
  name = "sentiment_result",
  strict = TRUE,
  schema = list(
    type = "object",
    properties = list(
      positive_phrases = list(
        type = "array",
        items = list(type = "string"),
        description = "Distinct phrases in the passage carrying positive financial sentiment. Each phrase is 1 to 5 words, taken from the passage."
      ),
      negative_phrases = list(
        type = "array",
        items = list(type = "string"),
        description = "Distinct phrases in the passage carrying negative financial sentiment. Each phrase is 1 to 5 words, taken from the passage."
      )
    ),
    required = list("positive_phrases", "negative_phrases"),
    additionalProperties = FALSE
  )
)

# The system prompt sets the rules once. The user message
# just carries the passage.
system_prompt <- paste0(
  "You are a financial sentiment analyst reading earnings call excerpts. ",
  "Identify phrases (1 to 5 words each) that carry positive or negative sentiment in a financial context. ",
  "Rules:\n",
  "1. Only include phrases that actually appear in the passage. Do not paraphrase or invent.\n",
  "2. Count each distinct phrase once. If the same phrase repeats, list it once.\n",
  "3. If a phrase is neutral, do not include it in either list.\n",
  "4. Positive examples in a finance context: 'record revenue', 'strong demand', 'raised guidance', 'margin expansion'.\n",
  "5. Negative examples in a finance context: 'declining sales', 'guidance cut', 'headwinds', 'weakness in'.\n",
  "6. Return only the JSON, with the two required arrays. Empty arrays are allowed when nothing qualifies."
)

# ============================================================
# ONE ROW, TO SEE THE MECHANIC
# ============================================================

# Before looping, run one call on the longest management turn
# so you can see exactly what comes back.
company_rows <- kept[kept$role_group == "Company", ]
if (nrow(company_rows) == 0) {
  stop("No management turns survived the length filter. Lower MIN_ROW_CHARS.")
}

demo_row <- company_rows[which.max(company_rows$msg_chars), ]

cat("---- Demo: scoring one turn ----\n")
cat("Speaker:", demo_row$speaker, "(", demo_row$title, ")\n")
cat("Length: ", demo_row$msg_chars, "characters\n")
cat("Preview:", substr(demo_row$msg, 1, 200), "...\n\n")

# scoreOneTurn: send one passage, get back the two phrase
# lists. This is the whole API contract in one function.
scoreOneTurn <- function(passage_text) {

  request_body <- list(
    model = MODEL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = passage_text)
    ),
    response_format = list(
      type = "json_schema",
      json_schema = sentiment_schema
    ),
    # require_parameters tells OpenRouter to route only to
    # providers that actually support the response_format we
    # asked for. Without this, a provider without json_schema
    # support will silently return prose.
    provider = list(
      require_parameters = TRUE
    )
  )

  req <- request(OPENROUTER_URL)
  req <- req_headers(req,
    "Authorization" = paste("Bearer", openrouter_key),
    "Content-Type" = "application/json"
  )
  req <- req_body_raw(req, toJSON(request_body, auto_unbox = TRUE), type = "application/json")
  req <- req_timeout(req, 120)
  # This is the line Ted asked for. Three tries, exponential
  # backoff, retries on transient failures automatically.
  req <- req_retry(req, max_tries = RETRY_MAX, backoff = ~ 2 ^ .x)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    err_txt <- resp_body_string(resp)
    stop(paste("API call failed:", substr(err_txt, 1, 300)))
  }

  parsed <- fromJSON(resp_body_string(resp), flatten = TRUE)
  raw_content <- parsed$choices$message.content[1]

  # The content is a JSON string, still. Parse it.
  result <- fromJSON(raw_content)

  # Defensive: if the model returned NULL for either field,
  # coerce to an empty character vector so downstream code
  # never sees NULL.
  if (is.null(result$positive_phrases) == TRUE) {
    result$positive_phrases <- character(0)
  }
  if (is.null(result$negative_phrases) == TRUE) {
    result$negative_phrases <- character(0)
  }

  return(result)
}

demo_result <- scoreOneTurn(demo_row$msg)

cat("Positive phrases (", length(demo_result$positive_phrases), "):\n", sep = "")
if (length(demo_result$positive_phrases) > 0) {
  for (p in demo_result$positive_phrases) {
    cat("  +", p, "\n")
  }
}

cat("Negative phrases (", length(demo_result$negative_phrases), "):\n", sep = "")
if (length(demo_result$negative_phrases) > 0) {
  for (p in demo_result$negative_phrases) {
    cat("  -", p, "\n")
  }
}
cat("\n")

# STOP AND LOOK AT THIS WHEN MAKING A PRODUCTIPN SYSTEM.
# Read the phrases against the passage. Are they actually
# there? Are they actually positive or negative? This is the
# only step where a human is looking at every phrase, and it
# is where you decide whether the model's judgment matches
# yours before you trust the loop.

# ============================================================
# LOOP OVER EVERY KEPT TURN
# ============================================================

n_kept <- nrow(kept)
cat("---- Scoring", n_kept, "turns ----\n")
cat("This will take a moment. One API call per turn.\n\n")

# Preallocate the result columns.
kept$positive_count <- integer(n_kept)
kept$negative_count <- integer(n_kept)
kept$positive_phrases <- character(n_kept)
kept$negative_phrases <- character(n_kept)
kept$msg_words <- integer(n_kept)

# Word count is done in R, not by the model. Splitting on
# whitespace is rough (it counts "don't" as one word and
# hyphenated compounds as one) but that is fine and it is
# reproducible.
countWords <- function(text_in) {
  pieces <- strsplit(text_in, "\\s+")[[1]]
  pieces <- pieces[nchar(pieces) > 0]
  return(length(pieces))
}

for (i in 1:n_kept) {

  # A one line progress indicator so students see it working.
  cat("[", i, "/", n_kept, "] ", substr(kept$speaker[i], 1, 30), "\n", sep = "")

  # Score. Because we used req_retry, transient errors are
  # already handled. If we get here with a hard error, wrap
  # it so one bad row does not kill the loop.
  scored <- tryCatch(
    scoreOneTurn(kept$msg[i]),
    error = function(e) {
      cat("  (row failed after retries: ", conditionMessage(e), ")\n", sep = "")
      return(list(positive_phrases = character(0), negative_phrases = character(0)))
    }
  )

  kept$positive_count[i] <- length(scored$positive_phrases)
  kept$negative_count[i] <- length(scored$negative_phrases)
  kept$positive_phrases[i] <- paste(scored$positive_phrases, collapse = ", ")
  kept$negative_phrases[i] <- paste(scored$negative_phrases, collapse = ", ")
  kept$msg_words[i] <- countWords(kept$msg[i])
}

cat("\nScoring complete.\n\n")

# ============================================================
# DERIVE THE PER ROW LABEL
# ============================================================

# sentiment_density is (positive minus negative) over the
# passage length. Positive numbers lean positive, negative
# numbers lean negative, and near zero means neutral. This is
# the arithmetic that Ted's example asked for.
kept$sentiment_density <- (kept$positive_count - kept$negative_count) / pmax(kept$msg_words, 1)

labelFromDensity <- function(d) {
  if (d > NEUTRAL_BAND) {
    return("positive")
  } else if (d < -NEUTRAL_BAND) {
    return("negative")
  } else {
    return("neutral")
  }
}

kept$label <- sapply(kept$sentiment_density, labelFromDensity)

# TIP: the label is derived, not asked for. That is the
# point. The model does the language part (finding phrases),
# and R does the arithmetic. Remember LLMs do not calculate in reality!

# ============================================================
# BUILD THE FINAL DATA FRAME
# ============================================================

sentiment_df <- data.frame(
  turn_index = kept$turn_index,
  symbol = SYMBOL,
  report_date = latest_date,
  speaker = kept$speaker,
  title = kept$title,
  role_group = kept$role_group,
  msg_chars = kept$msg_chars,
  msg_words = kept$msg_words,
  positive_count = kept$positive_count,
  negative_count = kept$negative_count,
  positive_phrases = kept$positive_phrases,
  negative_phrases = kept$negative_phrases,
  sentiment_density = round(kept$sentiment_density, 4),
  label = kept$label,
  stringsAsFactors = FALSE
)

nrow(sentiment_df)
names(sentiment_df)
head(sentiment_df)

# Explore by speaker; mgt is usually more positive
table(sentiment_df$speaker, sentiment_df$label)
aggregate(sentiment_density~speaker, sentiment_df, mean)
aggregate(sentiment_density~role_group, sentiment_df, mean)

# Keep in mind more sophisticated analysis can be done including length of statement
# often the "most negative analyst," is the person asking the sharpest questions
# and, on average, the one whose next earnings estimate is likely to move

# ============================================================
# SAVE
# ============================================================

# We save two objects: the per row data frame and a compact
# summary list. FIN_G reads both when it assembles the human
# review surface.

saveRDS(sentiment_df, file.path(savePth, "sentiment_scores.rds"))

sentiment_summary <- list(
  symbol = SYMBOL,
  report_date = latest_date,
  n_turns_scored = nrow(sentiment_df),
  overall = list(
    positive_count = sum(sentiment_df$label=='positive'),
    negative_count = sum(sentiment_df$label=='negative'),
    total_words    = sum(sentiment_df$msg_words),
    density        = round(sum(sentiment_df$sentiment_density), 4),
    label          = names(which.max((table(sentiment_df$label))))
  ),
  by_role = aggregate(sentiment_density~role_group, sentiment_df, mean),
  by_speaker = aggregate(sentiment_density~speaker, sentiment_df, mean)
)

saveRDS(sentiment_summary, file.path(savePth, "sentiment_summary.rds"))

cat("Saved sentiment_scores.rds and sentiment_summary.rds in\n", savePth, "\n\n")

cat("\n---- Learning Review ----\n")
cat("1. The model does the language work (finding phrases). R does the\n")
cat("   arithmetic (counts, density, labels).\n")
cat("2. Density weighting means a long speaker does not automatically\n")
cat("   dominate. Ten positive phrases in a hundred words carries more\n")
cat("   than ten in a thousand.\n")
cat("3. Per row calls are slow and clear. Batching is fast and opaque.\n")
cat("   Choose deliberately.\n")
cat("4. JSON schema removes an entire class of parsing bugs.\n")
cat("5. httr2's req_retry() makes a fifty call loop safe.\n")
cat("6. Open weight models are a fine fit for bounded classification\n")
cat("   like this. Save the frontier models for the tasks that need them.\n")

# End
