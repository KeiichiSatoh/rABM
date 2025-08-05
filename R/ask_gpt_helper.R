#' @title Helper Functions for ask_gpt
#' @description
#' A set of internal functions used by the \code{ask_gpt} workflow for RAG integration.
#' These include functions for reading remote files, cleaning code, and transforming
#' roxygen2 documentation into plain text.
#'
#' @name ask_gpt_helper
#' @keywords internal
#' @importFrom stringr str_starts str_remove str_replace str_detect
#' @importFrom purrr partial
#' @importFrom jsonlite fromJSON
#' @importFrom ragnar ragnar_store_create ragnar_store_insert ragnar_store_build_index MarkdownDocument markdown_chunk
NULL


#' Register RAG Store from GitHub Repository for GPT Use
#'
#' This internal function builds a local RAG (Retrieval-Augmented Generation) store
#' by fetching R package files from GitHub and embedding them using the selected embedding engine.
#'
#' @param gpt A character string specifying the GPT backend.
#'   One of `"openai"`, `"bedlock"`, `"databricks"`, `"google_vertex"`, or `"ollama"`.
#' @param embed_args A named list of arguments passed to the embedding function (e.g., model name).
#' @param store_location A character string specifying the DuckDB file path to store embeddings.
#'
#' @return Invisibly returns the created RAG store object.
#' Called primarily for its side effects.
#'
#' @details
#' The function fetches the contents of the GitHub repository `KeiichiSatoh/rABM`, including:
#' - R scripts (`/R`)
#' - C++ source files (`/src`)
#' - Unit tests (`/tests/testthat`)
#' - DESCRIPTION, NAMESPACE, NEWS.md, and README.md
#'
#' It uses `ragnar::MarkdownDocument()` and `markdown_chunk()` to chunk and structure these texts.
#' The embeddings are created using the appropriate `ragnar::embed_*()` function, partially applied with `embed_args`.
#' Finally, `ragnar_store_insert()` is used to populate the DuckDB store, and `ragnar_store_build_index()` is called to finalize it.
#'
#' @importFrom purrr partial
#' @importFrom jsonlite fromJSON
#' @importFrom ragnar ragnar_store_create ragnar_store_insert ragnar_store_build_index MarkdownDocument markdown_chunk
#' @keywords internal
.register_rag <- function(gpt, embed_args, store_location){
  embed_FUN <- switch(gpt,
                      "openai" = {embed_openai},
                      "bedlock" = {embed_bedrock},
                      "databricks" = {embed_databricks},
                      "google_vertex" = {embed_google_vertex},
                      "ollama" = {embed_ollama},
                      stop("Unsupported GPT engine: ", gpt))

  #------------------------------
  # get_URLs
  #------------------------------
  # API endpoint to list contents
  api_r <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/R"
  api_src <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/src"
  api_tests <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/tests/testthat"
  api_description <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/DESCRIPTION"
  api_namespace <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/NAMESPACE"
  api_news <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/NEWS.md"
  api_readme <- "https://api.github.com/repos/KeiichiSatoh/rABM/contents/README.md"


  # Get JSON listing
  url_r <- fromJSON(api_r)$download_url
  url_src <- fromJSON(api_src)$download_url
  url_tests <- fromJSON(api_tests)$download_url
  url_description <- fromJSON(api_description)$download_url
  url_namespace <- fromJSON(api_namespace)$download_url
  url_news <- fromJSON(api_news)$download_url
  url_readme <- fromJSON(api_readme)$download_url

  # change the arguments specified in embed_args
  # partial application: create a function with fixed arguments
  embed_wrapper <- purrr::partial(embed_FUN, !!!embed_args)

  store <- ragnar_store_create(
    location = store_location,
    overwrite = TRUE,
    embed = embed_wrapper
  )

  #--------------------------------------------------
  # get markdown documents for each type of documents
  #--------------------------------------------------

  # R scripts
  for(i in seq_along(url_r)){
    page <- safe_readLines(url_r[i]) |>
      .remove_code_comments() |> .roxygen_to_natural()
    chunks <- MarkdownDocument(text = page, origin = url_r[i]) |>
      markdown_chunk()
    ragnar_store_insert(store, chunks)
  }

  # src(parhaps, need to update in the future...)
  for(i in seq_along(url_src)){
    page <- safe_readLines(url_src[i]) |>
      .remove_code_comments() |> .roxygen_to_natural()
    chunks <- MarkdownDocument(text = page, origin = url_src[i]) |>
      markdown_chunk()
    ragnar_store_insert(store, chunks)
  }

  # tests
  for(i in seq_along(url_tests)){
    page <- safe_readLines(url_tests[i]) |>
      .remove_code_comments() |> .roxygen_to_natural()
    chunks <- MarkdownDocument(text = page, origin = url_tests[i]) |>
      markdown_chunk()
    ragnar_store_insert(store, chunks)
  }

  # description, namespace, news, readme
  url_others <- c(url_description, url_namespace, url_news, url_readme)
  for(i in seq_along(url_others)){
    page <- safe_readLines(url_others[i])
    chunks <- MarkdownDocument(text = page, origin = url_others[i]) |>
      markdown_chunk()
    ragnar_store_insert(store, chunks)
  }

  # create index
  ragnar_store_build_index(store)
}


#' Safely read content from a URL
#'
#' Attempts to read the full content from a given URL and collapse it into a single string.
#' If the connection fails, returns an empty string and issues a warning.
#'
#' @param url A character string representing the URL to read.
#' @return A character scalar containing the file content as a single string.
#' @keywords internal
safe_readLines <- function(url) {
  tryCatch(
    paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) {
      warning(paste("Failed to read:", url, "\n", conditionMessage(e)))
      return("")
    }
  )
}


#' Remove comments from R script (excluding roxygen2)
#'
#' Removes both full-line and inline comments from a given R script, but preserves roxygen2-style
#' comments (i.e., lines starting with \code{#'}).
#'
#' @param text A character scalar containing R script content.
#' @return A cleaned string with comments removed.
#' @keywords internal
.remove_code_comments <- function(text) {
  lines <- strsplit(text, "\n")[[1]]

  # remove comments (not excluding #')
  cleaned <- sapply(lines, function(line) {
    if (startsWith(trimws(line), "#'")) return(line)  # leave roxygen2 comments
    if (grepl("^\\s*#", line)) return("")             # remove lines if these are comments
    sub("#.*$", "", line)                             # remove inline comments
  })

  paste(cleaned[nzchar(cleaned)], collapse = "\n")
}


#' Convert roxygen2 comments into natural language format
#'
#' Transforms a block of roxygen2-formatted comments into readable plain text
#' by removing tags and cleaning syntax.
#'
#' @param text A character scalar representing roxygen2 documentation.
#' @return A plain-text version of the documentation suitable for embedding or search.
#' @keywords internal
.roxygen_to_natural <- function(text) {
  # Step 1: split into lines and remove "#'" prefix
  lines <- unlist(strsplit(text, "\n"))
  lines <- sub("^#' ?", "", lines)  # keep indentation after "#' "

  # Step 2: transform recognized tags line-by-line
  transformed <- character(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Skip blank lines
    if (nchar(trimws(line)) == 0) {
      transformed[i] <- ""
      next
    }

    # Handle specific roxygen tags
    if (str_starts(line, "@title")) {
      transformed[i] <- paste0("# Title: ", str_remove(line, "@title\\s*"))
    } else if (str_starts(line, "@description")) {
      transformed[i] <- paste0("# Description: ", str_remove(line, "@description\\s*"))
    } else if (str_starts(line, "@details")) {
      transformed[i] <- paste0("# Details: ", str_remove(line, "@details\\s*"))
    } else if (str_starts(line, "@param")) {
      transformed[i] <- stringr::str_replace(line, "@param\\s+(\\w+)", "Parameter '\\1':")
    } else if (str_starts(line, "@return")) {
      transformed[i] <- paste0("# Returns: ", str_remove(line, "@return\\s*"))
    } else if (str_starts(line, "@examples")) {
      transformed[i] <- "# Examples:"
    } else if (str_starts(line, "@import") || str_starts(line, "@importFrom")) {
      transformed[i] <- ""
    } else if (str_detect(line, "^@(?:export|seealso|keywords|aliases|encoding|docType|useDynLib|author)")) {
      transformed[i] <- ""
    } else {
      # Otherwise keep as-is (preserve indent)
      transformed[i] <- line
    }
  }

  # Step 3: rejoin lines and clean \code{}, \link{}, etc.
  all_text <- paste(transformed, collapse = "\n")

  all_text <- gsub("\\\\code\\{\\\\link\\{([^}]+)\\}\\}", "\\1", all_text)
  all_text <- gsub("\\\\code\\{([^}]+)\\}", "\\1", all_text)
  all_text <- gsub("\\\\link\\{([^}]+)\\}", "\\1", all_text)

  return(all_text)
}

