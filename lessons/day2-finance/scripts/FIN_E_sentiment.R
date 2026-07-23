# ============================================================
# FIN_E_sentiment.R
# Sentiment lesson, Day 2, Finance Masters track
# (9:00 to 10:00)
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

ARCHIVE_DIR <- "~/Desktop/vienna-genai-finance-course/earnings_call_archive/transcripts_sp500_marketbeat"

savePth <- "~/Desktop/vienna-genai-finance-course/context_files"

# The company to score. Change and re-run.
SYMBOL <- "AAPL"

OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Open weight, cheap, reliable at JSON schema. Kept as a top
# level constant so switching to a closed model (for example
# google/gemini-2.0-flash-001) is a one line change.
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
# other before we call it. Adjust and see how the labels
# shift.
NEUTRAL_BAND <- 0.005

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
# LOAD ONE CALL (same pattern as FIN_D)
# ============================================================

archive_path <- path.expand(ARCHIVE_DIR)
all_csv <- list.files(archive_path, pattern = "\\.csv$", full.names = TRUE)

if (length(all_csv) == 0) {
  stop("No CSV files found in the archive folder.")
}

symbol_prefix <- paste0(SYMBOL, "_")
name_starts <- substr(basename(all_csv), 1, nchar(symbol_prefix))
symbol_files <- all_csv[name_starts == symbol_prefix]

if (length(symbol_files) == 0) {
  stop(paste("No transcript files found for", SYMBOL, "in", archive_path))
}

file_dates <- substring(basename(symbol_files), nchar(symbol_prefix) + 1)
file_dates <- sub("\\.csv$", "", file_dates)

newest_idx <- order(file_dates, decreasing = TRUE)[1]
chosen_file <- symbol_files[newest_idx]
chosen_date <- file_dates[newest_idx]

this_call <- read.csv(chosen_file, stringsAsFactors = FALSE)

cat("Loaded", nrow(this_call), "speaker turns from", SYMBOL, "on", chosen_date, "\n\n")

# ============================================================
# TAG ROLES, DROP SHORT TURNS
# ============================================================

# Same role rule as FIN_A, so the split matches upstream.
isAnalyst <- function(title_text) {
  return(grepl("analyst|research", tolower(title_text)) == TRUE)
}

this_call$role_group <- ifelse(isAnalyst(this_call$title) == TRUE, "Analyst", "Company")

this_call$msg_chars <- nchar(this_call$msg)

kept <- this_call[this_call$msg_chars >= MIN_ROW_CHARS, ]

cat("---- Filtering short turns ----\n")
cat("Turns loaded:      ", nrow(this_call), "\n")
cat("Turns kept (>= ", MIN_ROW_CHARS, " chars): ", nrow(kept), "\n", sep = "")
cat("Turns dropped:     ", nrow(this_call) - nrow(kept), "\n\n")

# TIP: read the dropped turns before continuing. In one call
# they are almost all "Good morning" and "Thanks, next
# question." That is the right thing to drop. Occasionally
# something substantive is under the threshold and you should
# raise MIN_ROW_CHARS or lower it accordingly.

# Preserve the original turn index so it lines up with FIN_A.
kept$turn_index <- as.integer(rownames(kept))
rownames(kept) <- NULL

# ============================================================
# THE JSON SCHEMA
# ============================================================

# This is what OpenRouter passes to the provider. `strict`
# tells the provider to enforce the shape (a compatible
# provider will refuse to emit anything else). `additional
# Properties: false` blocks the model from inventing fields.

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

# STOP AND LOOK AT THIS.
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
# and R does the arithmetic. If a student later asks "why did
# this get called positive?", the answer is on this line, not
# hidden in a model.

# ============================================================
# BUILD THE FINAL DATA FRAME
# ============================================================

sentiment_df <- data.frame(
  turn_index = kept$turn_index,
  symbol = SYMBOL,
  report_date = chosen_date,
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

cat("---- Data frame built ----\n")
cat("Rows:", nrow(sentiment_df), "\n")
cat("Columns:", paste(names(sentiment_df), collapse = ", "), "\n\n")

cat("---- First 5 rows (label and counts) ----\n")
preview_cols <- c("speaker", "role_group", "msg_words", "positive_count", "negative_count", "label")
print(head(sentiment_df[, preview_cols], 5), row.names = FALSE)
cat("\n")

# ============================================================
# AGGREGATE: BY SPEAKER
# ============================================================

# Sum the counts and the words per speaker, then re-derive
# the label from the summed density. Averaging labels would
# lose the density weighting Ted asked for. Summing preserves
# it: a speaker who talked twice as long carries twice the
# weight in their own row.

aggregateGroup <- function(df, group_col) {
  groups <- unique(df[[group_col]])
  out <- data.frame(
    group = groups,
    n_turns = integer(length(groups)),
    total_words = integer(length(groups)),
    positive_count = integer(length(groups)),
    negative_count = integer(length(groups)),
    sentiment_density = numeric(length(groups)),
    label = character(length(groups)),
    stringsAsFactors = FALSE
  )

  for (i in 1:length(groups)) {
    rows <- df[df[[group_col]] == groups[i], ]
    out$n_turns[i] <- nrow(rows)
    out$total_words[i] <- sum(rows$msg_words)
    out$positive_count[i] <- sum(rows$positive_count)
    out$negative_count[i] <- sum(rows$negative_count)
    d <- (out$positive_count[i] - out$negative_count[i]) / max(out$total_words[i], 1)
    out$sentiment_density[i] <- round(d, 4)
    out$label[i] <- labelFromDensity(d)
  }

  return(out)
}

by_speaker <- aggregateGroup(sentiment_df, "speaker")

# Attach the role_group back for readability.
speaker_role_lookup <- unique(sentiment_df[, c("speaker", "role_group")])
by_speaker <- merge(by_speaker, speaker_role_lookup,
                    by.x = "group", by.y = "speaker",
                    all.x = TRUE)
names(by_speaker)[names(by_speaker) == "group"] <- "speaker"

# Sort by density so the most positive and most negative are
# at the ends.
by_speaker <- by_speaker[order(by_speaker$sentiment_density, decreasing = TRUE), ]

cat("---- Aggregated by speaker (sorted by density) ----\n")
print(by_speaker, row.names = FALSE)
cat("\n")

# ============================================================
# WHO WAS MOST POSITIVE, MOST NEGATIVE?
# ============================================================

managementOnly <- by_speaker[by_speaker$role_group == "Company", ]
analystOnly <- by_speaker[by_speaker$role_group == "Analyst", ]

cat("---- Extremes among management ----\n")
if (nrow(managementOnly) > 0) {
  cat("Most positive: ", managementOnly$speaker[1],
      " (density ", managementOnly$sentiment_density[1], ")\n", sep = "")
  cat("Most negative: ", managementOnly$speaker[nrow(managementOnly)],
      " (density ", managementOnly$sentiment_density[nrow(managementOnly)], ")\n", sep = "")
} else {
  cat("(no management turns)\n")
}
cat("\n")

cat("---- Extremes among analysts ----\n")
if (nrow(analystOnly) > 0) {
  cat("Most positive: ", analystOnly$speaker[1],
      " (density ", analystOnly$sentiment_density[1], ")\n", sep = "")
  cat("Most negative: ", analystOnly$speaker[nrow(analystOnly)],
      " (density ", analystOnly$sentiment_density[nrow(analystOnly)], ")\n", sep = "")
} else {
  cat("(no analyst turns)\n")
}
cat("\n")

# TIP: this is where the split by role earns its keep. The
# most positive voice on almost every call is a member of
# management. The interesting cell is "most negative analyst,"
# because that is the person asking the sharpest questions
# and, on average, the one whose next earnings estimate moves.

# ============================================================
# AGGREGATE: BY ROLE GROUP, THEN OVERALL
# ============================================================

by_role <- aggregateGroup(sentiment_df, "role_group")

cat("---- Aggregated by role group ----\n")
print(by_role, row.names = FALSE)
cat("\n")

overall_pos <- sum(sentiment_df$positive_count)
overall_neg <- sum(sentiment_df$negative_count)
overall_words <- sum(sentiment_df$msg_words)
overall_density <- (overall_pos - overall_neg) / max(overall_words, 1)
overall_label <- labelFromDensity(overall_density)

cat("---- Overall (whole call) ----\n")
cat("Total turns scored:", nrow(sentiment_df), "\n")
cat("Total words:      ", overall_words, "\n")
cat("Positive phrases: ", overall_pos, "\n")
cat("Negative phrases: ", overall_neg, "\n")
cat("Density:          ", round(overall_density, 4), "\n")
cat("Overall label:    ", overall_label, "\n\n")

# ============================================================
# SAVE
# ============================================================

# We save two objects: the per row data frame and a compact
# summary list. FIN_G reads both when it assembles the human
# review surface.

if (dir.exists(path.expand(savePth)) == FALSE) {
  dir.create(path.expand(savePth), recursive = TRUE)
}

sentiment_summary <- list(
  symbol = SYMBOL,
  report_date = chosen_date,
  n_turns_scored = nrow(sentiment_df),
  overall = list(
    positive_count = overall_pos,
    negative_count = overall_neg,
    total_words = overall_words,
    density = round(overall_density, 4),
    label = overall_label
  ),
  by_role = by_role,
  by_speaker = by_speaker
)

saveRDS(sentiment_df, file.path(savePth, "sentiment_scores.rds"))
saveRDS(sentiment_summary, file.path(savePth, "sentiment_summary.rds"))

cat("Saved sentiment_scores.rds and sentiment_summary.rds in\n", savePth, "\n\n")

# ============================================================
# NOW CHANGE SOMETHING
# ============================================================

cat("---- Try this ----\n")
cat("1. Raise MIN_ROW_CHARS to 200 and re-run. You will drop the\n")
cat("   quick answers and score only the substantial ones. Does the\n")
cat("   overall label change?\n")
cat("2. Widen NEUTRAL_BAND to 0.02. Watch how many rows move to neutral.\n")
cat("   The band is a choice, not a fact.\n")
cat("3. Change MODEL to a closed weight model, for example\n")
cat("   google/gemini-2.0-flash-001, re-run one turn, and compare the\n")
cat("   phrase lists. Is one more literal? Is one more aggressive?\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("\n---- Summary ----\n")
cat("1. The model does the language work (finding phrases). R does the\n")
cat("   arithmetic (counts, density, labels). The split is auditable.\n")
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
