# Set up an environment with an empty lookup table. This lookup can be populated
# from disk and/or during parsing.
.wikimorphemes_env <- new.env()
assign(
  "lookup",
  # By default, the lookup is empty to begin with. Various calls will populate
  # this with real data.
  tibble::tibble(
    word = character(0),
    morphemes = vector(mode = "list", length = 0),
    n_morphemes = integer(0)
  ),
  envir = .wikimorphemes_env
)

#' Populate the Environment Lookup with Saved Results
#'
#' @return Logical, invisibly; whether a cached lookup exists in that location.
#' @keywords internal
.populate_env_lookup <- function() {
  # nocov start; it's hard to specifically test this, partly because it's
  # memoised.
  cached_lookup <- .cache_lookup()
  if (!is.null(cached_lookup)) {
    if (
      !is.null(.wikimorphemes_env$lookup) && nrow(.wikimorphemes_env$lookup)
    ) {
      .wikimorphemes_env$lookup <- dplyr::bind_rows(
        .wikimorphemes_env$lookup,
        cached_lookup
      )
    } else {
      .wikimorphemes_env$lookup <- cached_lookup
    }
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
  # nocov end
}

#' Update the Environment Lookup
#'
#' When we look up a word and that word isn't in the existing lookup, we add it
#' to the lookup.
#'
#' @param word The word that has been processed.
#' @param morphemes The processed morphemes to save in the cache.
#' @inheritParams .process_word_recursive
#'
#' @return The morphemes, with any post-processing that's needed on all
#'   morphemes (right now, removing duplicate hyphens).
#' @keywords internal
.update_env_lookup <- function(word, morphemes, use_lookup) {
  # Get rid of double-hyphens here, since this is the function everything passes
  # through on the way out of the processor.
  morphemes <- rlang::set_names(
    stringr::str_replace_all(morphemes, "--", "-"),
    names(morphemes)
  )

  # Only update the lookup if they're using it.
  if (use_lookup) {
    word <- unname(word)

    .wikimorphemes_env$lookup <- dplyr::bind_rows(
      tibble::tibble(
        word = word,
        morphemes = list(.env$morphemes),
        n_morphemes = length(.env$morphemes)
      ),
      .wikimorphemes_env$lookup
    )
  }
  return(morphemes)
}

#' Pull a Word from the Lookup
#'
#' This is in a separate function because it's an area that we might be able to
#' optimize. I tried memoising, but that seemed to fight with and confuse the
#' cache (made things way slower).
#'
#' @inheritParams .process_word_recursive
#'
#' @return A character vector of morphemes, or a length-0 character vector if no
#'   morphemes are found.
#' @keywords internal
.pull_from_lookup <- function(word) {
  # The populate step only runs once per session, loading the data
  # from disk.
  .populate_env_lookup()

  this_row <- .wikimorphemes_env$lookup[
    .wikimorphemes_env$lookup$word == word,
  ]
  if (!is.null(this_row) && nrow(this_row)) {
    return(this_row$morphemes[[1]])
  } else {
    return(character(0))
  }
}

#' Reset the Lookup Cache
#'
#' Sometimes it is useful to reset the lookup cache, for example if you have
#' made a change on wiktionary and wish to use the newest result. This function
#' resets the in-memory lookup cache. This is likely a very rare situation.
#'
#' @return TRUE (invisibly) on success.
#' @export
#'
#' @examples
#' reset_lookup()
reset_lookup <- function() {
  .wikimorphemes_env$lookup <- NULL
  return(invisible(TRUE))
}

#' Save the Lookup Cache to Disk
#'
#' Unless you specify otherwise, wikimorphemes produces a cache in RAM of the
#' words you have parsed. Use this function to update (or create) an on-disk
#' version of this lookup table.
#'
#' @return TRUE, invisibly.
#' @export
save_wikimorphemes_lookup <- function() {
  # nocov start

  # Make sure any existing lookup is in the environment.
  .populate_env_lookup()

  # Make sure they have write permissions.
  filename <- .generate_cache_write_filename("wikimorphemes_lookup")

  saveRDS(
    dplyr::arrange(.wikimorphemes_env$lookup, .data$word),
    filename
  )

  return(invisible(TRUE))
  # nocov end
}
