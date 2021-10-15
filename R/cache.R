# Copyright 2021 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Wikimorphemes Cache Directory
#'
#' The wikimorphemes cache directory is a platform- and user-specific path where
#' wikimorpheme saves caches (such as a downloaded lookup). You can override the
#' default location in a few ways:
#' \itemize{
#'   \item{Option: \code{wikimorphemes.dir}}{Use
#'   \code{\link{set_wikimorphemes_cache_dir}} to set a specific cache directory
#'   for this session}
#'   \item{Environment: \code{WIKIMORPHEMES_CACHE_DIR}}{Set this environment
#'   variable to specify a wikimorphemes cache directory for all sessions.}
#'   \item{Environment: \code{R_USER_CACHE_DIR}}{Set this environment variable
#'   to specify a cache directory root for all packages that use the caching
#'   system.}
#' }
#'
#' @return A character vector with the normalized path to the cache.
#' @export
#'
#' @examples
#' wikimorphemes_cache_dir()
wikimorphemes_cache_dir <- function() {
  return(dlr::app_cache_dir("wikimorphemes"))
}

#' Return a Cached wikitext_en
#'
#' @return A wikitext_en tibble. Note that this tibble is copyright the
#'   Wikimedia Foundation, Inc, with a creative commons BY-SA license. See
#'   \url{https://foundation.wikimedia.org/wiki/Terms_of_Use/en} for more
#'   details.
#' @keywords internal
.cache_wikitext <- function() {
  cache_dir <- wikimorphemes_cache_dir()
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
  cache_dir <- wikimorphemes_cache_dir()
  cache_file <- fs::path(
    cache_dir,
    "wikimorphemes",
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

#' Load the Current Lookup
#'
#' During use, Wikimorphemes builds an in-memory lookup table of words and
#' morphemes. This table is used by \code{\link{process_word}}, but it might
#' also be useful for other uses.
#'
#' @return A tibble with columns word, morphemes (a list column with a character
#'   vector of morphemes per word), and n_morphemes (an integer count of
#'   morphemes in the morphemes column). Note that words less than 4 characters
#'   are never broken into morphemes by this package so they do not appear in
#'   the lookup. If you do not have a lookup, this function will return NULL.
#' @export
#'
#' @examples
#' nrow(wikimorphemes_lookup())
#' head(wikimorphemes_lookup())
wikimorphemes_lookup <- function() {
  # I separated this into a separate function to separate internal,
  # speed-optimized usage, with external, potentially print-optimized usage.
  return( # nocov start ; This gets tested via other functions right now.
    .cache_lookup()
  ) # nocov end
}

#' Set a Cache Directory for Wikimorphemes
#'
#' Use this function to override the cache path used by Wikimorphemes for the
#' current session. Set the \code{WIKIMORPHEMES_CACHE_DIR} environment variable
#' for a more permanent change.
#'
#' @param cache_dir Character scalar; a path to a cache directory.
#'
#' @return A normalized path to a cache directory. The directory is created if
#'   the user has write access and the directory does not exist.
#' @export
set_wikimorphemes_cache_dir <- function(cache_dir = NULL) {
  old_cache <- getOption("wikimorphemes.dir")
  cache_dir <- dlr::set_app_cache_dir(
    appname = "wikimorphemes", cache_dir = cache_dir
  )

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
    dlr::maybe_cache(
      source_path = this_url,
      appname = "wikimorphemes",
      filename = "wikimorphemes.rds"
    )
  } else {
    rlang::abort(
      message = "Only full lookups are currently available.",
      class = "lookup_style_error"
    )
  }
  # nocov end
}

#' Load the List of Words in Wiktionary
#'
#' Each time we process a dump from Wiktionary, we also save a separate list of
#' Wiktionary words. If you have already downloaded the word list, it will not
#' download again. Right now you need to manually delete the word list file to
#' remove it from your cache. Note: this will download a 4.12 MB file if you do
#' not already have it in your cache.
#'
#' @return A character vector with the unique words available in the English
#'   Wiktionary.
#' @export
wiktionary_word_list <- function() {
  return(
    dlr::read_or_cache(
      source_path = .wordlist_url,
      appname = "wikimorphemes",
      filename = "wiktionary_words.rds"
    )
  )
}
