#' Return a Cached wikitext_en
#'
#' @inheritParams wikimorphemes_cache_dir
#'
#' @return A wikitext_en tibble.
#' @keywords internal
.cache_wikitext <- function(cache_dir = wikimorphemes_cache_dir()) {
  cache_dir <- wikimorphemes_cache_dir(cache_dir)
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
#' @inheritParams wikimorphemes_cache_dir
#'
#' @return A tibble with columns word, morphemes (a list column of the breakdown
#'   returned by \code{\link{process_word}}), and n_morphemes (the length of the
#'   morphemes entry for this row).
#' @keywords internal
.cache_lookup <- function(cache_dir = wikimorphemes_cache_dir()) {
  cache_dir <- wikimorphemes_cache_dir(cache_dir)
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

#' Choose a Cache Directory for Wikimorphemes
#'
#' It is often useful to work with a local cache for this package. This function
#' helps to locate the cache directory. In most cases, you can just use the
#' default directory.
#'
#' @param cache_dir Character scalar; a path to a cache directory.
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist. Note that this
#'   function also sets an option, "wikimorphemes.dir", to simplify subsequent
#'   checks.
#' @export
wikimorphemes_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||%
    getOption("wikimorphemes.dir") %||%
    rappdirs::user_cache_dir(appname = "wikimorphemes")
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

  return(cache_dir)
}

#' Download a Wikimorphemes Lookup Table
#'
#' If you use this package a lot, we highly recommend downloading full dumps
#' occasionally, rather than constantly hitting the Wiktionary API.
#'
#' @param lookup_style Character; currently the only available size is "full",
#'   which is a 15MB rds file. I added this parameter to call out the size, and
#'   to soon have options with smaller lookups.
#' @inheritParams wikimorphemes_cache_dir
#'
#' @return TRUE (invisibly) on success.
#' @export
download_wikimorphemes_lookup <- function(lookup_style = "full",
                                        cache_dir = wikimorphemes_cache_dir()) {
  # nocov start; It's easier to just test this manually from time to time.
  cache_dir <- wikimorphemes_cache_dir(cache_dir)
  cache_file <- fs::path(
    cache_dir,
    "wikimorphemes_lookup",
    ext = "rds"
  )

  if (lookup_style == "full") {
    this_url <- .lookup_url
  } else {
    rlang::abort(
      message = "Only full lookups are currently available.",
      class = "lookup_style_error"
    )
  }

  status <- utils::download.file(
    url = .lookup_url,
    destfile = cache_file,
    method = "libcurl",
    mode = "wb"
  )
  if (status != 0) {
    stop("Lookup download failed.") # nocov
  }
  return(invisible(TRUE))
  # nocov end
}
