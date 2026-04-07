#-------------------------------------------------------------------------------
# add_chunk / remove_chunk / replace_chunk
#-------------------------------------------------------------------------------


#' Add Chunks to a Series Object
#'
#' Inserts one or more \code{Chunk} objects into an existing \code{Series}
#' object at a specified position.
#'
#' @param S A \code{Series} object.
#' @param ... One or more \code{Chunk} objects to insert.
#' @param after A single integer or character string specifying the position
#'   after which the new chunks are inserted. If a character string is given,
#'   it is matched against the names of the existing chunks. Cannot be
#'   specified together with \code{before}.
#' @param before A single integer or character string specifying the position
#'   before which the new chunks are inserted. If a character string is given,
#'   it is matched against the names of the existing chunks. Cannot be
#'   specified together with \code{after}.
#'
#' @return A \code{Series} object with the new chunks inserted.
#'
#' @details
#' If neither \code{after} nor \code{before} is specified, the new chunks
#' are appended to the end of the series.
#'
#' If \code{before = 1} or \code{after = 0} is specified, the new chunks
#' are inserted at the beginning of the series.
#'
#' Since \code{Series} is an S3 object, this function returns a modified
#' copy. The result must be assigned to take effect:
#' \code{S <- add_chunk(S, ...)}.
#'
#' @examples
#' s1 <- Chunk({ x <- 1 })
#' s2 <- Chunk({ y <- 2 })
#' S <- Series(s1, s2)
#'
#' s3 <- Chunk({ z <- 3 })
#' s4 <- Chunk({ w <- 4 })
#'
#' # append to the end
#' S <- add_chunk(S, s3, s4)
#'
#' # insert before the first chunk
#' S <- add_chunk(S, s3, s4, before = 1)
#'
#' # insert after the first chunk by name
#' S <- add_chunk(S, s3, s4, after = "s1")
#'
#' @seealso \code{\link{remove_chunk}}, \code{\link{replace_chunk}}
#'
#' @export
add_chunk <- function(S, ..., after = NULL, before = NULL){

  # validate after/before
  if (!is.null(after) && !is.null(before)) {
    stop("Only one of 'after' or 'before' can be specified.")
  }
  if (!is.null(after) && length(after) != 1L) stop("'after' must be a single value.")
  if (!is.null(before) && length(before) != 1L) stop("'before' must be a single value.")

  S_temp <- Series(...)

  # get the current chunk names
  nms <- names(S$chunks)

  # get the position
  if (!is.null(after)) {
    if (is.character(after)) {
      end_posit <- match(after, nms)
      if (is.na(end_posit)) stop("'after' = \"", after, "\" does not match any chunk name.")
    } else {
      end_posit <- after
    }
  } else if (!is.null(before)) {
    if (is.character(before)) {
      end_posit <- match(before, nms) - 1L
      if (is.na(end_posit)) stop("'before' = \"", before, "\" does not match any chunk name.")
    } else {
      end_posit <- before - 1L
    }
  } else {
    end_posit <- length(nms)
  }

  # validate position
  stopifnot("The insert position is not correct." = !is.na(end_posit) && end_posit >= 0)
  stopifnot("The insert position exceeds the length of the current `Series`." = end_posit <= length(nms))

  # create new chunks
  if (end_posit == 0) {
    new_chunks <- c(S_temp$chunks, S$chunks)
  } else {
    new_chunks <- c(S$chunks[1:end_posit], S_temp$chunks)
    if (end_posit < length(nms)) {
      new_chunks <- c(new_chunks, S$chunks[(end_posit + 1):length(nms)])
    }
  }

  # replace the current chunks
  S$chunks <- new_chunks

  S
}




#' Remove Chunks from a Series Object
#'
#' Removes one or more \code{Chunk} objects from an existing \code{Series}
#' object by name.
#'
#' @param S A \code{Series} object.
#' @param ... One or more character strings specifying the names of the
#'   chunks to remove.
#'
#' @return A \code{Series} object with the specified chunks removed.
#'
#' @details
#' Since \code{Series} is an S3 object, this function returns a modified
#' copy. The result must be assigned to take effect:
#' \code{S <- remove_chunk(S, ...)}.
#'
#' @examples
#' s1 <- Chunk({ x <- 1 })
#' s2 <- Chunk({ y <- 2 })
#' S <- Series(s1, s2)
#'
#' # remove a single chunk
#' S <- remove_chunk(S, "s1")
#'
#' # remove multiple chunks
#' S <- remove_chunk(S, "s1", "s2")
#'
#' @seealso \code{\link{add_chunk}}, \code{\link{replace_chunk}}
#'
#' @export


remove_chunk <- function(S, ...){
  remove_nms <- unlist(list(...))

  # validate input
  stopifnot("All chunk names must be character strings." = is.character(remove_nms))

  # chunk names
  check <- match(remove_nms, names(S$chunks))
  if(any(is.na(check))){
    stop("The following chunk names do not exist: ",
         paste(remove_nms[is.na(check)], collapse = ", "))
  }

  # delete
  for(nm in remove_nms){
    S$chunks[[nm]] <- NULL
  }

  # Return
  S
}


#' Replace Chunks in a Series Object
#'
#' Replaces one or more \code{Chunk} objects in an existing \code{Series}
#' object. The names of the new chunks must match those of the chunks to
#' be replaced.
#'
#' @param S A \code{Series} object.
#' @param ... One or more \code{Chunk} objects to replace. Each chunk must
#'   have a name that matches an existing chunk in \code{S}.
#'
#' @return A \code{Series} object with the specified chunks replaced.
#'
#' @details
#' Since \code{Series} is an S3 object, this function returns a modified
#' copy. The result must be assigned to take effect:
#' \code{S <- replace_chunk(S, ...)}.
#'
#' @examples
#' s1 <- Chunk({ x <- 1 })
#' s2 <- Chunk({ y <- 2 })
#' S <- Series(s1, s2)
#'
#' # replace chunks
#' s1 <- Chunk({ x <- 10 })
#' s2 <- Chunk({ y <- 20 })
#' S2 <- replace_chunk(S, s1, s2)
#'
#' @seealso \code{\link{add_chunk}}, \code{\link{remove_chunk}}
#'
#' @export
replace_chunk <- function(S, ...){
  S_temp <- Series(...)

  # Retrieve the chunk names
  chunk_nms_temp <- names(S_temp$chunks)
  chunk_nms      <- names(S$chunks)
  check <- match(chunk_nms_temp, chunk_nms)
  if(any(is.na(check))){
    stop("The following chunk names do not exist: ",
         paste(chunk_nms_temp[is.na(check)], collapse = ", "))
  }

  # replace
  for(nm in chunk_nms_temp){
    S$chunks[[nm]] <- S_temp$chunks[[nm]]
  }

  # return
  S
}
