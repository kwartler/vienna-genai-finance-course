#' Author: Ted Kwartler 
#' An example for students using OpenRouter.ai cloud API for model calls.
#' July 14 2026

library(httr2)
OPENROUTER_API_KEY <- Sys.getenv("OPENROUTER_API_KEY")

# ---- Edit these ----
systemPrompt    <- "You are a helpful AI assistant"
userPrompt      <- "What is the Apple stock ticker?"
# Grounding demo prompt (use with the search model below):
# userPrompt    <- "What was Apple's most recent quarterly revenue?"
# userPrompt    <- "Give me a random number between 1-50."
maxTokens       <- 1024
temperature     <- 0.7
openRouterModel <- 'openai/gpt-4.1-nano'   
# meta-llama/llama-3.2-1b-instruct
# openai/gpt-4.1-nano
# deepseek/deepseek-v4-flash
# minimax/minimax-m3
# z-ai/glm-5.2,
# meta-llama/llama-3.2-1b-instruct
# google/gemini-3.1-flash-lite
# anthropic/claude-haiku-4.5
# perplexity/sonar # works for citation
# openai/gpt-5.6-luna

# Models that generate validated JSON otherwise MUST BE FALSE
#   google/gemini-2.5-flash-lite   
#   openai/gpt-4.1-nano   
jsonOutput      <- F 

# ---- Do not change below ----
systemContent <- if(jsonOutput==T){
  paste(systemPrompt, "Respond only with valid JSON.")
} else {
  systemPrompt
}

requestBody <- list(
  model = openRouterModel,
  messages = list(
    list(role = "system", content = systemContent),
    list(role = "user",   content = userPrompt)
  ),
  max_tokens = maxTokens,
  temperature = temperature
)
if (jsonOutput == TRUE) {
  requestBody$response_format <- list(type = "json_object")
}
response <- request("https://openrouter.ai/api/v1/chat/completions") |>
  req_headers(Authorization = paste("Bearer", OPENROUTER_API_KEY)) |>
  req_body_json(requestBody) |>
  req_perform()

# The message holds both the text answer and any grounding citations
message     <- resp_body_json(response)$choices[[1]]$message
llmResponse <- message$content

if (jsonOutput == TRUE) {
  cat(jsonlite::prettify(llmResponse))
} else {
  cat(llmResponse)
}

# Print grounding URLS them when present. 
annotations <- message$annotations
if (length(annotations) > 0) {
  cat("\n\nSources:\n")
  for (a in annotations) {
    cat("- ", a$url_citation$title, ": ", a$url_citation$url, "\n", sep = "")
  }
}
# End