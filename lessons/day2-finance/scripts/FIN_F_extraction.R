# ============================================================
# FIN_F_extraction.R
# NER and extraction lesson, Day 2, Finance Masters track
#
# Purpose: pull structured facts out of an earnings call. From
# each speaker turn we extract five categories: companies,
# executives, products, financial figures with values, and
# forward looking statements. Everything comes back in a JSON
# object with the shape we asked for.
#
# How to use this script: run it a block at a time and read
# what prints. The first row is the interesting one, again.
# Watch how the model separates a company from a product from
# a person, then follow the loop.
#
# WHAT'S THE SAME AS FIN_E.
# httr2 with retry, JSON schema validation, a fast classroom
# model by default, per row calls, one call per turn. The pedagogy
# from FIN_E carries. If you skipped FIN_E, read its header.
#
# WHAT'S NEW HERE.
#
# 1. A larger schema. Five fields instead of two, some of them
#    arrays of strings and some of them arrays of objects.
#    Schemas can nest, and the model will fill in a shape it
#    has never seen as long as the shape is described clearly.
#
# 2. An example in the system prompt. For sentiment, "positive
#    phrase" is intuitive. For extraction, "financial figure"
#    could mean many things (a raw number, a percentage, a
#    dollar amount tied to a metric). A worked example inside
#    the prompt pins down what we mean. Read it below and
#    notice how much interpretation it removes.
#
# 3. Two output shapes. A per turn data frame that you can
#    read row by row (who said what), and a call level
#    aggregate that dedupes entities and lists every figure
#    and every forward looking statement with its speaker.
#    FIN_G reads the aggregate.
#
# THE FIVE CATEGORIES, IN PLAIN LANGUAGE.
# companies:                 named third parties (competitors,
#                            customers, partners). NOT the
#                            reporting company itself.
# executives:                named people, with their role.
# products:                  distinct product lines, services,
#                            or business segments.
# financial_figures:         a number tied to what it measures.
#                            "$51 billion" alone is not enough,
#                            "$51 billion in iPhone revenue" is.
# forward_looking_statements: any claim about the future.
#                            "we expect", "we guide", "next
#                            quarter", "into 2026".
# ============================================================

# ---- Libraries ----
library(httr2)
library(jsonlite)

# ============================================================
# CONFIGURATION
# ============================================================

ARCHIVE_DIR <- file.path("earnings_call_archive", "transcripts_sp500_marketbeat")

savePth <- "context_files"

SYMBOL <- "AAPL"

# The reporting company. Passed to the model so it knows NOT
# to list itself in the companies field. Without it the model dutifully returns
# "Apple" for every turn of an Apple call...which is not helpful!  
REPORTING_COMPANY <- "Apple"

OPENROUTER_URL <- "https://openrouter.ai/api/v1/chat/completions"

# Fast default for a 100-plus-call classroom exercise. Set
# OPENROUTER_ROW_MODEL to compare another model without editing.
MODEL <- Sys.getenv("OPENROUTER_ROW_MODEL", unset = "google/gemini-3.5-flash-lite")

# Same length filter as FIN_E: nothing meaningful to extract
# from a "thanks, next question."
MIN_ROW_CHARS <- 40

RETRY_MAX <- 5

# ============================================================
# THE API KEY
# ============================================================

openrouter_key <- Sys.getenv("OPENROUTER_API_KEY")

if (nchar(openrouter_key) == 0) {
  stop("No OpenRouter key. Set OPENROUTER_API_KEY in .Renviron and restart R.")
}

# ============================================================
# LOAD ONE CALL (same pattern as FIN_D and FIN_E)
# ============================================================

symbol_files <- list.files(ARCHIVE_DIR,
                           pattern = paste0("^", SYMBOL, "_.*\\.csv$"),
                           full.names = TRUE)

file_dates <- substring(basename(symbol_files), nchar(SYMBOL) + 2)
file_dates <- sub("\\.csv$", "", file_dates)

newest_idx <- order(file_dates, decreasing = TRUE)[1]
chosen_file <- symbol_files[newest_idx]
chosen_date <- file_dates[newest_idx]

this_call <- read.csv(chosen_file, stringsAsFactors = FALSE)

cat("Loaded", nrow(this_call), "speaker turns from", SYMBOL, "on", chosen_date, "\n\n")

# Same role rule.
isAnalyst <- grepl("analyst|research|managing director", 
                   this_call$title, 
                   ignore.case = T)
this_call$role_group <- ifelse(isAnalyst==T, "Analyst","Company")
this_call$msg_chars <- nchar(this_call$msg)

kept <- this_call[this_call$msg_chars >= MIN_ROW_CHARS, ]

cat("---- Filtering short turns ----\n")
cat("Turns loaded: ", nrow(this_call), "\n")
cat("Turns kept:   ", nrow(kept), "\n\n")

kept$turn_index <- as.integer(rownames(kept))
rownames(kept) <- NULL

# ============================================================
# THE JSON SCHEMA
# ============================================================

# Five fields. Three are arrays of strings, two are arrays of
# small objects. The nested objects (executives, financial
# figures) also declare additionalProperties = FALSE so the
# model cannot add fields we did not ask for.

extraction_schema <- list(
  name = "extraction_result",
  strict = TRUE,
  schema = list(
    type = "object",
    properties = list(

      companies = list(
        type = "array",
        items = list(type = "string"),
        description = "Distinct third party companies mentioned by name. Do NOT include the reporting company itself."
      ),

      executives = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            name = list(type = "string"),
            role = list(type = "string")
          ),
          required = list("name", "role"),
          additionalProperties = FALSE
        ),
        description = "Named individuals with their role. Role may be a job title (CEO) or a description (analyst at Morgan Stanley)."
      ),

      products = list(
        type = "array",
        items = list(type = "string"),
        description = "Distinct products, services, or business segments named in the passage."
      ),

      financial_figures = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            figure = list(type = "string"),
            metric = list(type = "string")
          ),
          required = list("figure", "metric"),
          additionalProperties = FALSE
        ),
        description = "Numeric values tied to what they measure. A bare number is not enough. Each figure must be paired with a metric."
      ),

      forward_looking_statements = list(
        type = "array",
        items = list(type = "string"),
        description = "Statements that describe the future: guidance, expectations, projections, plans. Use the words from the passage."
      )

    ),
    required = list("companies", "executives", "products",
                    "financial_figures", "forward_looking_statements"),
    additionalProperties = FALSE
  )
)

# ============================================================
# THE SYSTEM PROMPT (with a worked example)
# ============================================================

# Read the example carefully. It does more work than any
# description could. It shows exactly how the model should
# treat "Apple" (as the reporting company, so excluded from
# companies), how to pair figures with metrics, and how to
# quote a forward looking statement verbatim.

system_prompt <- paste0(
  "You are a financial text extraction system. You will receive a passage from an earnings call for ",
  REPORTING_COMPANY, ".\n\n",
  "Extract five categories from the passage. Rules:\n",
  "1. Use only what is present in the passage. Do not infer, do not add outside knowledge.\n",
  "2. Do NOT list ", REPORTING_COMPANY, " under 'companies'. Only OTHER companies belong there.\n",
  "3. Every financial figure must be paired with a metric. A bare number does not qualify.\n",
  "4. Forward looking statements are quoted from the passage, not paraphrased.\n",
  "5. If a category is empty for this passage, return an empty array.\n\n",
  "EXAMPLE INPUT:\n",
  "\"We had an exceptional September quarter. iPhone revenue reached $51.3 billion, up 6 percent year over year, ",
  "and Services set an all-time record of $24.2 billion. Tim Cook commented that ",
  "customer response to the new lineup has been strong. Looking ahead, we expect December quarter ",
  "revenue growth to accelerate compared to the September quarter. In the enterprise segment, ",
  "we continue to see Apple platforms displace Windows PCs at major customers including Deloitte.\"\n\n",
  "EXAMPLE OUTPUT:\n",
  "{\n",
  "  \"companies\": [\"Microsoft\", \"Deloitte\"],\n",
  "  \"executives\": [{\"name\": \"Tim Cook\", \"role\": \"CEO\"}],\n",
  "  \"products\": [\"iPhone\", \"Services\", \"Windows PCs\"],\n",
  "  \"financial_figures\": [\n",
  "    {\"figure\": \"$51.3 billion\", \"metric\": \"iPhone revenue\"},\n",
  "    {\"figure\": \"6 percent\", \"metric\": \"iPhone year over year growth\"},\n",
  "    {\"figure\": \"$24.2 billion\", \"metric\": \"Services revenue\"}\n",
  "  ],\n",
  "  \"forward_looking_statements\": [\n",
  "    \"we expect December quarter revenue growth to accelerate compared to the September quarter\"\n",
  "  ]\n",
  "}\n\n",
  "Notice how Microsoft was inferred from 'Windows PCs' even though not named directly. Do that ONLY when the connection is unambiguous. When in doubt, leave it out."
)

# TIP: that example is doing three things at once. It defines
# each field. It shows the shape of the JSON. And it settles
# hard edges (the reporting company rule, the figure-plus-
# metric rule, the quote-verbatim rule) with a concrete case
# rather than a paragraph of instructions. Every difficult
# extraction call benefits from an example in the prompt.

# ============================================================
# THE CALL FUNCTION
# ============================================================

extractOneTurn <- function(passage_text) {

  request_body <- list(
    model = MODEL,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = passage_text)
    ),
    response_format = list(
      type = "json_schema",
      json_schema = extraction_schema
    ),
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
  req <- req_retry(req,
                   max_tries = RETRY_MAX,
                   retry_on_failure = TRUE)

  resp <- req_perform(req)

  if (resp_status(resp) != 200) {
    err_txt <- resp_body_string(resp)
    stop(paste("API call failed:", substr(err_txt, 1, 300)))
  }

  parsed <- fromJSON(resp_body_string(resp), flatten = TRUE)
  raw_content <- parsed$choices$message.content[1]

  # simplifyVector = FALSE keeps the nested objects (executives
  # and financial_figures) as lists of named lists, which is
  # exactly what we want for downstream aggregation. Without
  # this flag jsonlite would coerce them into data frames
  # that behave strangely when a row has zero entries.
  result <- fromJSON(raw_content, simplifyVector = FALSE)

  # Defensive: coerce NULL to empty list so downstream code
  # never sees NULL.
  required_fields <- c("companies", "executives", "products",
                       "financial_figures", "forward_looking_statements")
  for (fld in required_fields) {
    if (is.null(result[[fld]]) == TRUE) {
      result[[fld]] <- list()
    }
  }

  return(result)
}

# ============================================================
# ONE ROW, TO SEE THE MECHANIC
# ============================================================

company_rows <- kept[kept$role_group == "Company", ]
if (nrow(company_rows) == 0) {
  stop("No management turns survived the length filter.")
}

demo_row <- company_rows[which.max(company_rows$msg_chars), ]

cat("---- Demo: extracting one turn ----\n")
cat("Speaker:", demo_row$speaker, "(", demo_row$title, ")\n")
cat("Length: ", demo_row$msg_chars, "characters\n")
cat("Preview:", substr(demo_row$msg, 1, 200), "...\n\n")

demo_result <- extractOneTurn(demo_row$msg)

# Print each category cleanly so students can read the shape.
cat('Executives:\n', paste0(demo_result$executives, collapse = '\n'))
cat('Products:\n', paste0(demo_result$products, collapse = '\n'))
cat('Companies:\n', paste0(demo_result$companies, collapse = '\n'))
cat('Financial figures:\n', paste0(demo_result$financial_figures, collapse = '\n'))
cat('Forward looking:\n', paste0(demo_result$forward_looking_statements, collapse = '\n'))


# STOP AND LOOK.
# Compare the passage against what came back. Two questions:
# (1) is anything missing that you can see with your own eyes?
# (2) is anything included that does not match the definitions?
# Both errors are informative. Under extraction usually means
# the model was too conservative and the prompt needs sharper
# examples. Over extraction usually means the definitions are
# too loose. You adjust the system prompt, not the model.

# ============================================================
# LOOP OVER EVERY KEPT TURN - run these code blocks 
# ============================================================

n_kept <- nrow(kept)
cat("---- Extracting", n_kept, "turns ----\n")
cat("One API call per turn.\n\n")

# We collect two things in parallel:
# 1. A per turn data frame with counts and readable summary
#    strings (for View() in RStudio and for eyeballing).
# 2. A per turn LIST that keeps the full nested structure,
#    for programmatic aggregation and for FIN_G to consume.

kept$companies_count <- integer(n_kept)
kept$executives_count <- integer(n_kept)
kept$products_count <- integer(n_kept)
kept$financial_figures_count <- integer(n_kept)
kept$forward_looking_count <- integer(n_kept)

kept$companies_list <- character(n_kept)
kept$executives_list <- character(n_kept)
kept$products_list <- character(n_kept)
kept$financial_figures_list <- character(n_kept)
kept$forward_looking_list <- character(n_kept)

per_turn_full <- vector("list", n_kept)
api_failed <- logical(n_kept)

# Helpers to flatten nested items into a semicolon separated
# string for the readable columns. Semicolons because commas
# appear inside role and metric strings.

flattenExecutives <- function(exec_list) {
  if (length(exec_list) == 0) {
    return("")
  }
  parts <- character(length(exec_list))
  for (i in 1:length(exec_list)) {
    parts[i] <- paste0(exec_list[[i]]$name, " (", exec_list[[i]]$role, ")")
  }
  return(paste(parts, collapse = "; "))
}

flattenFigures <- function(fig_list) {
  if (length(fig_list) == 0) {
    return("")
  }
  parts <- character(length(fig_list))
  for (i in 1:length(fig_list)) {
    parts[i] <- paste0(fig_list[[i]]$figure, " for ", fig_list[[i]]$metric)
  }
  return(paste(parts, collapse = "; "))
}

flattenStrings <- function(str_list) {
  if (length(str_list) == 0) {
    return("")
  }
  return(paste(unlist(str_list), collapse = "; "))
}

for (i in 1:n_kept) {

  cat("[", i, "/", n_kept, "] ", substr(kept$speaker[i], 1, 30), "\n", sep = "")

  row_failed <- FALSE
  extracted <- tryCatch(
    extractOneTurn(kept$msg[i]),
    error = function(e) {
      row_failed <<- TRUE
      cat("  (row failed after retries: ", conditionMessage(e), ")\n", sep = "")
      return(list(
        companies = list(),
        executives = list(),
        products = list(),
        financial_figures = list(),
        forward_looking_statements = list()
      ))
    }
  )

  api_failed[i] <- row_failed

  per_turn_full[[i]] <- extracted

  kept$companies_count[i] <- length(extracted$companies)
  kept$executives_count[i] <- length(extracted$executives)
  kept$products_count[i] <- length(extracted$products)
  kept$financial_figures_count[i] <- length(extracted$financial_figures)
  kept$forward_looking_count[i] <- length(extracted$forward_looking_statements)

  kept$companies_list[i] <- flattenStrings(extracted$companies)
  kept$executives_list[i] <- flattenExecutives(extracted$executives)
  kept$products_list[i] <- flattenStrings(extracted$products)
  kept$financial_figures_list[i] <- flattenFigures(extracted$financial_figures)
  kept$forward_looking_list[i] <- flattenStrings(extracted$forward_looking_statements)
}

cat("\nExtraction complete.\n\n")

if (any(api_failed)) {
  stop(sum(api_failed),
       " row(s) failed after retries. No extraction files were saved; rerun when the API is available.")
}

# ============================================================
# BUILD THE PER TURN DATA FRAME
# ============================================================

extraction_df <- data.frame(
  turn_index = kept$turn_index,
  symbol = SYMBOL,
  report_date = chosen_date,
  speaker = kept$speaker,
  title = kept$title,
  role_group = kept$role_group,
  companies_count = kept$companies_count,
  executives_count = kept$executives_count,
  products_count = kept$products_count,
  financial_figures_count = kept$financial_figures_count,
  forward_looking_count = kept$forward_looking_count,
  companies_list = kept$companies_list,
  executives_list = kept$executives_list,
  products_list = kept$products_list,
  financial_figures_list = kept$financial_figures_list,
  forward_looking_list = kept$forward_looking_list,
  stringsAsFactors = FALSE
)

cat("---- Per turn data frame ----\n")
nrow(extraction_df)
names(extraction_df)

# Show the counts across the first several rows.
preview_cols <- c("speaker", "role_group",
                  "companies_count", "executives_count", "products_count",
                  "financial_figures_count", "forward_looking_count")
cat("---- First 5 rows (counts) ----\n")
print(head(extraction_df[, preview_cols], 5), row.names = FALSE)
cat("\n")

# ============================================================
# AGGREGATE ACROSS THE WHOLE CALL
# ============================================================

# For companies, executives, and products: dedupe. These are
# entities that get named many times. One list per category
# is what a downstream reader is usually expecting.
#
# For financial figures and forward looking statements: do
# NOT dedupe. Every figure and every statement is a separate
# claim, and losing the speaker attribution would matter.
# We keep them all and attach the speaker.

# ---- Dedupe entities ----

all_companies <- character(0)
all_products <- character(0)
exec_names <- character(0)
exec_roles <- character(0)

for (i in 1:n_kept) {
  full_i <- per_turn_full[[i]]

  if (length(full_i$companies) > 0) {
    all_companies <- c(all_companies, unlist(full_i$companies))
  }
  if (length(full_i$products) > 0) {
    all_products <- c(all_products, unlist(full_i$products))
  }
  if (length(full_i$executives) > 0) {
    for (ex in full_i$executives) {
      exec_names <- c(exec_names, ex$name)
      exec_roles <- c(exec_roles, ex$role)
    }
  }
}

unique_companies <- sort(unique(all_companies))
unique_products <- sort(unique(all_products))

# For executives, dedupe on name and take the first observed role.
if (length(exec_names) > 0) {
  exec_df <- data.frame(name = exec_names, role = exec_roles, stringsAsFactors = FALSE)
  exec_df <- exec_df[duplicated(exec_df$name) == FALSE, ]
  rownames(exec_df) <- NULL
} else {
  exec_df <- data.frame(name = character(0), role = character(0), stringsAsFactors = FALSE)
}

# ---- Collect figures and forward looking with attribution ----

all_figures <- data.frame(
  turn_index = integer(0),
  speaker = character(0),
  role_group = character(0),
  figure = character(0),
  metric = character(0),
  stringsAsFactors = FALSE
)

all_fls <- data.frame(
  turn_index = integer(0),
  speaker = character(0),
  role_group = character(0),
  statement = character(0),
  stringsAsFactors = FALSE
)

for (i in 1:n_kept) {
  full_i <- per_turn_full[[i]]

  if (length(full_i$financial_figures) > 0) {
    for (fg in full_i$financial_figures) {
      all_figures <- rbind(all_figures, data.frame(
        turn_index = kept$turn_index[i],
        speaker = kept$speaker[i],
        role_group = kept$role_group[i],
        figure = fg$figure,
        metric = fg$metric,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (length(full_i$forward_looking_statements) > 0) {
    for (fls in full_i$forward_looking_statements) {
      all_fls <- rbind(all_fls, data.frame(
        turn_index = kept$turn_index[i],
        speaker = kept$speaker[i],
        role_group = kept$role_group[i],
        statement = fls,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# ============================================================
# SHOW THE AGGREGATE
# ============================================================

cat("---- Unique companies mentioned (", length(unique_companies), ") ----\n", sep = "")
if (length(unique_companies) > 0) {
  cat(paste(unique_companies, collapse = ", "), "\n")
} else {
  cat("(none)\n")
}

cat("---- Unique executives (", nrow(exec_df), ") ----\n", sep = "")
if (nrow(exec_df) > 0) {
  print(exec_df, row.names = FALSE)
} else {
  cat("(none)\n")
}

cat("---- Unique products and segments (", length(unique_products), ") ----\n", sep = "")
if (length(unique_products) > 0) {
  cat(paste(unique_products, collapse = ", "), "\n")
} else {
  cat("(none)\n")
}

cat("---- Financial figures (", nrow(all_figures), ", not deduped) ----\n", sep = "")
if (nrow(all_figures) > 0) {
  # Show the first ten to keep console output reasonable.
  print(head(all_figures[, c("speaker", "figure", "metric")], 10), row.names = FALSE)
  if (nrow(all_figures) > 10) {
    cat("... plus", nrow(all_figures) - 10, "more.\n")
  }
} else {
  cat("(none)\n")
}

cat("---- Forward looking statements (", nrow(all_fls), ") ----\n", sep = "")
if (nrow(all_fls) > 0) {
  # These deserve the full print so you can read them.
  for (i in 1:nrow(all_fls)) {
    cat("[", all_fls$speaker[i], "] ", all_fls$statement[i], "\n", sep = "")
  }
} else {
  cat("(none)\n")
}

# TIP: the forward looking list is the single most useful
# output of this script for someone building an investment
# thesis. Everything else is what happened. This is what
# management is telling you is about to happen, in their
# words. 

# ============================================================
# SAVE
# ============================================================

extraction_summary <- list(
  symbol = SYMBOL,
  report_date = chosen_date,
  n_turns_extracted = nrow(extraction_df),
  unique_companies = unique_companies,
  unique_executives = exec_df,
  unique_products = unique_products,
  financial_figures = all_figures,
  forward_looking_statements = all_fls
)

saveRDS(extraction_df, file.path(savePth, "extraction_per_turn.rds"))
saveRDS(extraction_summary, file.path(savePth, "extraction_summary.rds"))
saveRDS(per_turn_full, file.path(savePth, "extraction_per_turn_full.rds"))

cat("Saved:\n")
cat("  extraction_per_turn.rds       (data frame, readable columns)\n")
cat("  extraction_summary.rds        (deduped and attributed aggregate)\n")
cat("  extraction_per_turn_full.rds  (raw nested lists per turn)\n")
cat("in", savePth, "\n\n")

# ============================================================
# NOW CHANGE SOMETHING
# ============================================================

cat("---- Try this ----\n")
cat("1. Remove the worked example from the system prompt, re-run\n")
cat("   one turn, and compare. The example does more work than you\n")
cat("   would think.\n")
cat("2. Set REPORTING_COMPANY to \"XYZ\" and re-run one turn. Watch\n")
cat("   the reporting company start appearing in the companies list.\n")
cat("   That is why we told the model who NOT to list.\n")
cat("3. Loosen the financial figure rule (drop the metric requirement),\n")
cat("   and see how many bare numbers get returned.\n\n")

# ============================================================
# WHAT YOU SHOULD TAKE AWAY
# ============================================================

cat("---- Learning Review ----\n")
cat("1. A JSON schema can nest. Arrays of objects are how you carry\n")
cat("   related fields together (name plus role, figure plus metric).\n")
cat("2. A worked example in the system prompt outperforms a paragraph\n")
cat("   of rules for anything the definitions cannot fully pin down.\n")
cat("3. Dedupe entities, keep every claim. Companies and products get\n")
cat("   named many times. Figures and forward looking statements are\n")
cat("   each unique claims with a speaker behind them.\n")
cat("4. Attribution matters. \"The CFO said Q4 revenue would grow\" is\n")
cat("   information. \"Q4 revenue would grow\" is a rumor.\n")
cat("5. Forward looking statements are the single most decision-relevant\n")
cat("   output. They are guidance in the speaker's own words.\n")

# End
