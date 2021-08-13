#' Return a Cached wikitext_en
#'
#' @return A wikitext_en tibble.
#' @keywords internal
.cache_wikitext <- function() {
  cache_dir <- getOption("wikimorphemes.dir")
  cache_file <- fs::path(
    cache_dir,
    "wikitext_en",
    ext = "rds"
  )
  if (file.exists(cache_file)) {
    return(
      readRDS(cache_file)
    )
  } else {
    return(NULL)
  }
}

#' Return a Cached Lookup Table
#'
#' @return A tibble with columns word, morphemes (a list column of the breakdown
#'   returned by \code{\link{process_word}}), and n_morphemes (the length of the
#'   morphemes entry for this row).
#' @keywords internal
.cache_lookup <- function() {
  cache_dir <- getOption("wikimorphemes.dir")
  cache_file <- fs::path(
    cache_dir,
    "wikimorphemes_lookup",
    ext = "rds"
  )
  if (file.exists(cache_file)) {
    return(
      readRDS(cache_file)
    )
  } else {
    return(NULL)
  }
}

#' Set a Cache Directory for Wikimorphemes
#'
#' By default, this package uses a cache directory chosen using
#' /code{/link[dlr]{app_cache_dir}}. If you wish to set a different directory,
#' call this function. This function is also called when the package loads. If
#' you wish to always set a different cache directory, you can set the
#' environment variable WIKIMORPHEMES_CACHE_DIR or the option wikimorphemes.dir.
#' Note that this function sets an option when called.
#'
#' @param cache_dir Character scalar; a path to a cache directory.
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist.
#' @export
set_wikimorphemes_cache_dir <- function(cache_dir = NULL) {
  # All of this will be handled via {dlr} in the near future. I'm doing it here
  # as a proof of concept before largely moving it there.
  old_cache <- getOption("wikimorphemes.dir")
  cache_env <- Sys.getenv("WIKIMORPHEMES_CACHE_DIR")
  if (cache_env == "") cache_env <- NULL
  cache_dir <- cache_dir %||%
    old_cache %||%
    cache_env %||%
    dlr::app_cache_dir(appname = "wikimorphemes")
  cache_dir <- normalizePath(cache_dir)

  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE) # nocov
  } else {
    # file.access returns 0 for success because it hates clean code. The user
    # has to have read permissions for this function.
    if (file.access(cache_dir, 4) != 0) { # nocov start
      rlang::abort(
        message = paste("You do not have read access to", cache_dir),
        class = "dir_read_error"
      )
    } # nocov end
  }

  options(wikimorphemes.dir = cache_dir)

  # If it changed, we need to deal with memoised things.
  if (!is.null(old_cache) && cache_dir != old_cache) {
    memoise::forget(.cache_wikitext) # nocov start
    memoise::forget(.cache_lookup)
    memoise::forget(.populate_env_lookup)

    # And then we should add anything that's in the new lookup to the existing
    # environment lookup.
    .populate_env_lookup() # nocov end
  }

  return(cache_dir)
}

#' Download a Wikimorphemes Lookup Table
#'
#' If you use this package a lot, we highly recommend downloading full dumps
#' occasionally, rather than constantly hitting the Wiktionary API.
#'
#' @param lookup_style Character; currently the only available size is "full",
#'   which is a 10.7MB rds file. I added this parameter to call out the size,
#'   and to soon have options with smaller lookups.
#'
#' @return TRUE (invisibly) on success.
#' @export
download_wikimorphemes_lookup <- function(lookup_style = "full") {
  # nocov start; It's easier to just test this manually from time to time.
  if (lookup_style == "full") {
    this_url <- .lookup_url
    dlr::download_cache(
      url = this_url,
      appname = "wikimorphemes",
      filename = "wikimorphemes_lookup.rds"
    )
  } else {
    rlang::abort(
      message = "Only full lookups are currently available.",
      class = "lookup_style_error"
    )
  }
  # nocov end
}
