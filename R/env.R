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
#' @inheritParams wikimorphemes_cache_dir
#'
#' @return Logical, invisibly; whether a cached lookup exists in that location.
#' @keywords internal
.populate_env_lookup <- function(cache_dir = wikimorphemes_cache_dir()) {
  # nocov start; it's hard to specifically test this, partly because it's
  # memoised.
  cached_lookup <- .cache_lookup(cache_dir)
  if (!is.null(cached_lookup)) {
    if (nrow(.wikimorphemes_env$lookup)) {
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
.pull_from_lookup <- function(word, cache_dir) {
  # The populate step only runs once per session per cache_dir, loading the data
  # from disk.
  .populate_env_lookup(cache_dir)

  this_row <- .wikimorphemes_env$lookup[
    .wikimorphemes_env$lookup$word == word,
  ]
  if (nrow(this_row)) {
    return(this_row$morphemes[[1]])
  } else {
    return(character(0))
  }
}

#' Save the Lookup Cache to Disk
#'
#' Unless you specify otherwise, wikimorphemes produces a cache in RAM of the
#' words you have parsed. Use this function to update (or create) an on-disk
#' version of this lookup table.
#'
#' @inheritParams wikimorphemes_cache_dir
#'
#' @return TRUE, invisibly.
#' @export
save_wikimorphemes_lookup <- function(cache_dir = wikimorphemes_cache_dir()) {
  # nocov start
  # Make sure cache_dir is well-formed.
  cache_dir <- wikimorphemes_cache_dir(cache_dir)

  # Make sure any existing lookup in this location is in the environment.
  .populate_env_lookup(cache_dir)

  saveRDS(
    .wikimorphemes_env$lookup,
    fs::path(wikimorphemes_cache_dir(), "wikimorphemes_lookup", ext = "rds")
  )

  return(invisible(TRUE))
  # nocov end
}
