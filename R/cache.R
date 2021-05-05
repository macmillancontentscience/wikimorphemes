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
