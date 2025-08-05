#' @title Ask GPT with Code-Aware Retrieval-Augmented Generation (RAG)
#'
#' @description
#' This function allows users to interactively query an LLM (via `ellmer`),
#' using Retrieval-Augmented Generation (RAG) based on the content of the rABM
#' It fetches and embeds relevant package files (R scripts, tests, docs), stores them locally,
#' and initiates a chat loop with automatic retrieval from the embedded store.
#'
#' @param question A character string. The initial question to start the conversation.
#' @param gpt A character string indicating which GPT engine to use.
#'   One of `"openai"`, `"bedlock"`, `"databricks"`, `"google_vertex"`, or `"ollama"`.
#' @param overwrite_rag Logical. If `TRUE`, the existing local knowledge store (DuckDB file) will be rebuilt. Default is `FALSE`.
#' @param system_prompt Optional character vector of additional system prompts. Default is `NULL`.
#' @param include_default_system_prompt Logical. If `TRUE` (default),
#' a default prompt is included to guide the assistant's behavior as an R programming expert.
#' @param gpt_args A named list of arguments to be passed to the `chat_*` function
#'  (e.g., model name, temperature, etc.).
#' @param embed_args A named list of arguments to pass to the embedding function.
#'   For example, `list(model = "text-embedding-3-small")`.
#'
#' @return
#' A data frame with two columns:
#' \describe{
#'   \item{question}{User-provided queries}
#'   \item{answer}{Model-generated responses}
#' }
#'
#' @details
#' This function supports RAG-based querying by building or loading a local
#' DuckDB knowledge store.
#' The store is constructed from a rABM's GitHub repository containing R package source files,
#' and embeddings are created via `ragnar::embed_*()` functions.
#'
#' During the REPL-like interaction, relevant content is retrieved from the store
#'  using `ragnar_retrieve()`
#' and passed into the prompt context. The interaction continues until
#' the user types `"END"`.
#'
#' If the knowledge store does not exist and `overwrite_rag = FALSE`,
#' the user is prompted to build it.
#' The resulting DuckDB file is named as `"rABM_ragnar_<gpt>_duckdb"` and
#' saved to the working directory.
#'
#' @section Important Prerequisite:
#' You must have a valid API key for the selected GPT backend.
#' For example, if you use OpenAI's ChatGPT, visit:
#' https://platform.openai.com/
#'
#' to generate a key under Dashboard > API Keys.
#'
#' Note that running this function may incur OpenAI API charges.
#'
#' You may temporarily register the key via:
#'
#' \code{Sys.setenv(OPENAI_API_KEY = "sk-...")}
#'
#' Alternatively, you can store the API key in your .Renviron file to make it persist across sessions.
#'
#' As a more convenient option, you can also pass the API key directly as an argument
#' In this case, set your api_key as arguments to gpt_args and embed_args:
#'
#' \code{ask_gpt(question, gpt_args = list(api_key = ...), embed_args = list(api_key = ...))}
#'
#' @seealso
#' \href{https://ellmer.tidyverse.org/}{ellmer} for chat-related functionality. \cr
#' \href{https://ragnar.tidyverse.org/}{ragnar} for RAG-related functionality.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_starts str_remove str_replace
#'
#' @importFrom ragnar ragnar_store_create
#' @importFrom ragnar ragnar_store_insert
#' @importFrom ragnar ragnar_store_build_index
#' @importFrom ragnar ragnar_store_connect
#' @importFrom ragnar ragnar_retrieve
#' @importFrom ragnar ragnar_register_tool_retrieve
#' @importFrom ragnar MarkdownDocument markdown_chunk
#'
#' @importFrom ellmer chat_openai chat_bedrock chat_databricks chat_google_vertex chat_ollama
#' @importFrom ragnar embed_openai embed_bedrock embed_databricks embed_google_vertex embed_ollama
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ask_gpt(
#'   question = "How do I initialize agents in rABM?",
#'   gpt = "openai",
#'   system_prompt = "Please answer in Japanese."
#' )
#' }

ask_gpt <- function(
  question,
  gpt = c("openai", "bedlock","databricks","google_vertex","ollama"),
  overwrite_rag = FALSE,
  system_prompt = NULL,
  include_default_system_prompt = TRUE,
  gpt_args = list(),
  embed_args = list(model = "text-embedding-3-small")
  ){
  #------------------------------
  # validation
  #------------------------------
  # question
  stopifnot(
    "'question' must be a character vector of length 1." =
      is.character(question) && length(question) == 1
  )

  # gpt-model
  gpt <- match.arg(gpt)
  chat_FUN <- switch(gpt,
                     "openai" = {chat_openai},
                     "bedlock" = {chat_bedrock},
                     "databricks" = {chat_databricks},
                     "google_vertex" = {chat_google_vertex},
                     "ollama" = {chat_ollama},
                     stop("Unsupported GPT engine: ", gpt))

  # store_location name
  store_location <- paste0("rABM_", "ragnar_", gpt, "_duckdb")

  # include_default_system_prompt
  if(include_default_system_prompt){
    default_system_prompt <- str_squish(
    "
    You are an expert R programmer and mentor. Keep your responses concise.
    Before answering, retrieve relevant information from the knowledge store.
    Quote or paraphrase passages clearly, and distinguish your own words from the source material.
    You may also use your general knowledge of R programming,
    but indicate explicitly when your response is not based on the knowledge store.
    ")
  }else{
    default_system_prompt <- NULL
  }
  combined_system_prompt <- c(default_system_prompt, system_prompt)

  #-------------------------------
  # check the existence of duckdb
  #-------------------------------
  is_duckdb_exist <- store_location %in% list.files(getwd())

  # create duckdb
  if(!is_duckdb_exist){
    message("'", store_location, "' does not exist in your current working directory. ",
            "It is necessary for using this function. Can we create it?")
    answer <- readline("> [y/n]")
    if(answer == "y"){
      cat("Building the knowledge store. This may take a while. Please wait...\n")
      .register_rag(gpt = gpt,
                    embed_args = embed_args,
                    store_location = store_location)
    }else{
      print("The execution is interrupted.")
      return(NULL)
    }
  }

  # overwrite duckdb
  if(overwrite_rag){
    message("'", store_location, "' will be overwritten. ",
            "Are you sure to proceed?")
    answer <- readline("> [y/n]")
    if(answer == "y"){
      cat("Building the knowledge store. This may take a while. Please wait...\n")
      .register_rag(gpt = gpt,
                    embed_args = embed_args,
                    store_location = store_location)
    }else{
      print("The execution is interrupted.")
      return(NULL)
    }
  }

  #-------------------------------
  # instance the chat
  #-------------------------------
  chat <- do.call(chat_FUN, args = c(list(system_prompt = combined_system_prompt),
                                     gpt_args))

  #-------------------------------
  # connect to store_location
  #-------------------------------
  store <- ragnar_store_connect(store_location, read_only = TRUE)

  #-------------------------------
  # create a log object
  #-------------------------------
  # initiate empty log
  chat_log <- data.frame(question = character(), answer = character(), stringsAsFactors = FALSE)

  #-------------------------------
  # get relevant_chunks
  #-------------------------------
  relevant_chunks <- ragnar_retrieve(store, question)

  # register chat to ragner
  ragnar_register_tool_retrieve(chat, store, top_k = 10)

  # first response
  answer <- chat$chat(question)
  chat_log <- rbind(chat_log, data.frame(question = question, answer = answer, stringsAsFactors = FALSE))
  print(answer)

  # REPL loop
  repeat {
    cat("\n---\nType your next message (or type 'END' to finish):\n")
    question <- readline(">>> ")

    if (tolower(trimws(question)) == "end") {
      cat("Conversation ended. Returning the full chat log.\n")
      break
    }

    if (trimws(question) == "") {
      cat("No input detected. Please enter a message or type 'END' to finish.\n")
      next
    }

    # Retrieve and update store
    relevant_chunks <- ragnar_retrieve(store, question)
    ragnar_register_tool_retrieve(chat, store, top_k = 10)

    # Chat and update log
    answer <- chat$chat(question)
    chat_log <- rbind(chat_log,
                      data.frame(question = question, answer = answer, stringsAsFactors = FALSE))

    print(answer)
  }

  # return log silently
  invisible(chat_log)
} #//---END OF FUNCTION---//

