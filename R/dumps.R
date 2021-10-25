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

# For now, we aren't going to test *any* of this. I test it manually.

# nocov start

#' Process the Latest Wiktionary Dump File
#'
#' Using this package can result in a lot of hits to the wiktionary API. To
#' reduce that burden, this function downloads and processes the entire
#' wiktionary dump, to create a local version of the English words from the
#' latest dump.
#'
#' This function requires a lot of RAM and is very slow. It took about 4 hrs to
#' complete on a machine with 64GB of RAM.
#'
#' Note that the data generated in this process is copyright the Wikimedia
#' Foundation, Inc, with a creative commons BY-SA license. See
#' \url{https://foundation.wikimedia.org/wiki/Terms_of_Use/en} for more details.
#'
#' @return A logical scalar (invisibly) indicating whether the data was updated.
#' @keywords internal
.create_wikitext_en <- function() {
  cache_filename <- .generate_cache_write_filename(filename = "wikitext_en")
  dump_date <- .get_latest_dump_date()

  if (.cache_up_to_date(cache_filename, dump_date)) {
    rlang::inform(
      "The existing cache is already up to date.",
      class = "no_new_dump"
    )
    return(invisible(FALSE))
  }

  # If they made it here, download the dump to a temp file.
  cat("\nDownloading latest wiktionary dump.\n")
  dump_filename <- .download_latest_dump()

  # This is slow, but gets us the info to allow us to logically go through the
  # dump. MIGHT be an issue on systems with less RAM than mine, needs to be
  # tested.
  cat("\nParsing page info.\n")
  page_info <- .find_page_info(dump_filename)

  cat("\nParsing wikitext_en.\n")
  wikitext_en <- .parse_dump(dump_filename, page_info)
  attr(wikitext_en, "wt_update_date") <- dump_date

  saveRDS(wikitext_en, cache_filename)
  return(invisible(TRUE))
}

#' Generate Filenames for Wikitext Cache Files
#'
#' @param filename Character scalar; the filename minus the extension, such as
#'   "wikitext_en".
#' @param ext Character scalar; the file extension (default "rds").
#'
#' @return A character scalar validated path for writing.
#' @keywords internal
.generate_cache_write_filename <- function(filename, ext = "rds") {
  cache_dir <- wikimorphemes_cache_dir()
  cache_dir <- .validate_cache_dir_write()

  return(
    fs::path(
      cache_dir,
      filename,
      ext = ext
    )
  )
}

#' Make Sure the Cache Dir is Writable
#'
#' @return The cache_dir, if it is writable.
#' @keywords internal
.validate_cache_dir_write <- function() {
  cache_dir <- wikimorphemes_cache_dir()
  # Check that they have read/write on the cache path. I don't validate the path
  # otherwise since I export the function that does so.
  if (file.access(cache_dir, 3) != 0) {
    rlang::abort(
      message = paste("You do not have write access to", dir),
      class = "dir_write_error"
    )
  }
  return(cache_dir)
}

#' Check wiktionary for the Date of the Latest Dump
#'
#' @return The dump date as a POSIXct object, in GMT.
#' @keywords internal
.get_latest_dump_date <- function() {
  return(
    lubridate::parse_date_time(
      xml2::xml_text(
        xml2::xml_find_all(
          xml2::read_xml(
            paste0(
              "https://dumps.wikimedia.org/enwiktionary/latest/",
              "enwiktionary-latest-pages-articles.xml.bz2-rss.xml"
            )
          ),
          ".//pubDate"
        )
      ),
      "a, d b Y H:M:S",
      tz = "GMT"
    )
  )
}

#' Check if the Cache is Up to Date
#'
#' @param cache_filename Character scalar; the full path to the (possible)
#'   cache.
#' @param dump_date POSIXct scalar; the date against which the cache should be
#'   compared.
#'
#' @return Logical scalar indicating whether the cache exists and is at least as
#'   new as the latest dump.
#' @keywords internal
.cache_up_to_date <- function(cache_filename, dump_date) {
  if (file.exists(cache_filename)) {
    # Check the date on the existing cache.
    wikitext_en <- readRDS(cache_filename)
    wt_update_date <- attr(wikitext_en, "wt_update_date")

    return(
      !is.null(wt_update_date) && dump_date <= wt_update_date
    )
  } else {
    return(FALSE)
  }
}

#' Download the Latest Wiktionary Dump
#'
#' @return The path to the dump tempfile.
#' @keywords internal
.download_latest_dump <- function() {
  dump_filename <- tempfile("wiktionary_dump", fileext = ".xml.bz2")
  download_check <- utils::download.file(
    url = paste0(
      "https://dumps.wikimedia.org/enwiktionary/latest/",
      "enwiktionary-latest-pages-articles.xml.bz2"
    ),
    destfile = dump_filename,
    quiet = TRUE
  )
  if (download_check == 0) {
    return(dump_filename)
  } else {
    # download.file SHOULD error but in case it errors silently do this.
    rlang::abort(
      message = paste(
        "Download failed with download.file error code",
        download_check
      ),
      class = "download_error"
    )
  }
}

#' Create a Tibble of Page Info
#'
#' @param dump_filename Character scalar; the full path to a dump tempfile.
#'
#' @return A tibble of rows with \code{<page>} tags, and information about the
#'   spacing between them.
#' @keywords internal
.find_page_info <- function(dump_filename) {
  return(
    dplyr::mutate(
      tibble::tibble(
        start_line = grep("<page>", readLines(dump_filename))
      ),
      end_line = dplyr::lead(.data$start_line - 1L),
      total_lines = .data$end_line - .data$start_line + 1L
    )
  )
}

#' Parse the Dump File into a Wikitext Tibble
#'
#' @inheritParams .find_page_info
#' @param page_info The page_info tibble generated by
#'   \code{\link{.find_page_info}}.
#'
#' @return A tibble of English wikitext entries.
#' @keywords internal
.parse_dump <- function(dump_filename, page_info) {
  # Open a connec/tion. We'll continuously read from this until we reach the
  # end, parsing as we go.
  con <- file(dump_filename, open = "r")
  on.exit(close(con), add = TRUE)

  # There will often be gaps (at least at the start). It's useful to keep track
  # of what line we're on so we can read in the gaps.
  lines_loaded <- 0L

  total_rows <- nrow(page_info)
  word_info <- vector(mode = "list", length = total_rows)

  show_progress <- requireNamespace("progress", quietly = TRUE)
  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "  processed *:current* of :total pages (:percent) eta: :eta",
      total = total_rows,
      clear = FALSE,
      width = 60
    )
  }

  # Since it's difficult (logistically) to start/stop this function, I don't
  # return a partial result on.exit. That contrasts with .create_lookup, where I
  # *can* logically start/stop the process and pick up wherever I was. This
  # difference is due to the nature of the gigantic temp file that is read
  # piece-by-piece during this process.

  # Now read page, which we can do simply by reading the number of lines in each
  # total_lines entry. This should be a for loop because we need each one to
  # read after the previous one, so the connection advances.
  for (i in seq_along(page_info$total_lines)) {
    if (show_progress & i %% 10000 == 0) {
      pb$tick(10000)
    }

    next_line <- page_info$start_line[[i]]

    # There will be garbage at the start. Jump forward.
    skip_lines <- next_line - lines_loaded - 1L

    if (skip_lines > 0) {
      throwaway <- readLines(con, skip_lines)
      lines_loaded <- lines_loaded + skip_lines
    }

    n_lines <- page_info$total_lines[[i]]

    # The last page doesn't have n_lines, so deal with that.
    this_page_vector <- if (is.na(n_lines)) {
      readLines(con, encoding = "UTF-8")
    } else {
      readLines(con, n_lines, encoding = "UTF-8")
    }

    lines_loaded <- lines_loaded + length(this_page_vector)

    # For the xml to be readable, we need to end with "</page>".
    last_page <- grep("</page>", this_page_vector)

    # If there isn't a last_page we can't deal with this.
    if (length(last_page)) {
      this_page <- paste(this_page_vector[1:last_page], collapse = "\n")

      # Check if this is an article or something else.
      this_xml <- xml2::read_xml(this_page)
      ns <- xml2::xml_text(
        xml2::xml_find_first(
          this_xml,
          ".//ns"
        )
      )

      # ns 0 is an actual article.
      if (ns == 0L) {
        # Attempt to just get the <text> block from this page to avoid weird
        # situations. If there isn't a text block, it looks like that means the
        # page has been intentionally blanked out. I've seen one case of this
        # ever but now we deal with it.
        this_wt <- stringr::str_extract(
          this_page,
          stringr::regex("<text.+</text>", dotall = TRUE)
        )
        if (!is.na(this_wt)) {
          this_entry <- .extract_relevant_english_wt(this_wt)
          if (length(this_entry)) {
            title <- xml2::xml_text(
              xml2::xml_find_first(
                this_xml,
                ".//title"
              )
            )
            sha1 <- xml2::xml_text(
              xml2::xml_find_first(
                this_xml,
                ".//sha1"
              )
            )
            word_info[[i]] <- data.frame(
              row_n = i,
              word = title,
              wikitext = this_entry,
              sha1 = sha1
            )
          }
        }
      }
    }
  }

  wikitext_en <- dplyr::as_tibble(
    dplyr::bind_rows(word_info)
  )

  # Protect against duplicates with blank data.
  repeats <- dplyr::filter(
    dplyr::count(wikitext_en, .data$row_n),
    .data$n > 1
  )

  if (nrow(repeats)) { # nocov start
    # For the one example I saw of this, the repeat was empty wikitext. So let's
    # just get rid of that.
    wikitext_en <- dplyr::filter(
      wikitext_en,
      !(
        .data$row_n %in% repeats$row_n &
          .data$wikitext == ""
      )
    )
  } # nocov end

  return(wikitext_en)
}

#' Generate the Lookup for All Words in Dump
#'
#' The wikitext dump contains information about all words on wiktionary. This
#' function runs each of those words through process_word, generating a new
#' lookup in the process. The lookup is then saved to disk.
#'
#' @inheritParams process_word
#'
#' @return TRUE (invisibly), via save_wikimorphemes_lookup on.exit.
#' @keywords internal
.create_lookup <- function(sight_words = default_sight_words()) {
  # Don't bother doing any of this if they can't write.
  .validate_cache_dir_write()

  # As we process words, the lookup is automatically generated. We'll start with
  # all words in the cached wikitext.
  words <- .cache_wikitext()$word

  # Get rid of sight_words and anything that has already been added to the
  # lookup.
  words <- words[!(words %in% sight_words)]
  total_words <- length(words)

  # Make sure the saved lookup is loaded.
  .populate_env_lookup()
  words <- words[!(words %in% .wikimorphemes_env$lookup$word)]

  # Only show the progress bar if {progress} is installed, and don't require
  # installation if they don't have it.
  show_progress <- requireNamespace("progress", quietly = TRUE)
  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "  processed *:current* of :total words (:percent) eta: :eta",
      total = total_words,
      clear = FALSE,
      width = 60
    )
  }

  # We want to save whatever we finish if the process stops early. This is
  # useful so we can run this in pieces.
  on.exit(
    save_wikimorphemes_lookup()
  )

  for (i in seq_along(words)) {
    if (show_progress & i %% 1000 == 0) {
      pb$tick(1000)
    }
    process_word(
      word = words[[i]],
      sight_words = sight_words,
      use_lookup = TRUE
    )
  }
}

#' Generate the List of Wiktionary Words
#'
#' This is simply the unique words from \code{\link{.cache_wikitext}}, sorted.
#'
#' @return TRUE (invisibly) on success.
#' @keywords internal
.create_word_lists <- function() {
  all_words <- sort(unique(.cache_wikitext()$word))
  saveRDS(
    all_words,
    .generate_cache_write_filename("wiktionary_words")
  )
  writeLines(
    all_words,
    con = .generate_cache_write_filename("wiktionary_words", "txt")
  )
  return(invisible(TRUE))
}

#' Download the Latest Dump and Create Lookup
#'
#' This function wraps \code{\link{.create_wikitext_en}} and
#' \code{\link{.create_lookup}} to download the latest wiktionary dump and
#' process it into a lookup.
#'
#' @inheritParams .create_lookup
#'
#' @return A logical scalar (invisibly) indicating whether the data was updated.
#' @keywords internal
.create_cache_files <- function(sight_words = default_sight_words()) {
  if (.create_wikitext_en()) {
    .create_lookup(sight_words = sight_words)
    .create_word_lists()
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}

# nocov end
