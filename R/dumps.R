.download_dump <- function(file_path) {
  # We don't really want to *keep* the downloads, but instead just use them then
  # delete them, so I'm not exporting this function.
  download.file(
    url = "https://dumps.wikimedia.org/enwiktionary/latest/enwiktionary-latest-pages-articles.xml.bz2",
    destfile = file_path,
    quiet = TRUE
  )
}

process_wiktionary_dump <- function(cache_dir = wikimorphemes_cache_dir()) {
  cache_dir <- .validate_cache_dir_write(cache_dir)

  # I'm going to need the dump date whether or not they already have a cache.
  dump_date <- .get_latest_dump_date()

  # Check if they already have a cache.
  cache_filename <- fs::path(
    cache_dir,
    "wikitext_en",
    ext = "rds"
  )

  if (.cache_up_to_date(cache_filename, dump_date)) {
    rlang::inform(
      "The existing cache is already up to date.",
      class = "no_new_dump"
    )
    return(invisible(dump_date))
  }

  # If they made it here, download the dump to a temp file.
  dump_filename <- .download_latest_dump()

  # This is slow, but gets us the info to allow us to logically go through the
  # dump. MIGHT be an issue on systems with less RAM than mine, needs to be
  # tested.
  page_info <- .find_page_info(dump_filename)

  wikitext_en <- .parse_dump(dump_filename, page_info)
  attr(wikitext_en, "wt_update_date") <- dump_date

  saveRDS(wikitext_en, cache_filename)
  return(invisible(dump_date))
}

wikimorphemes_cache_dir <- function(dir = NULL) {
  dir <- dir %||%
    getOption("wikimorphemes.dir") %||%
    rappdirs::user_cache_dir(appname = "wikimorphemes")
  dir <- normalizePath(dir)
  if (!file.exists(dir)) {
    dir.create(dir, recursive = TRUE) # nocov
  } else {
    # file.access returns 0 for successs because it hates clean code. The user
    # has to have read permissions for this function.
    if (file.access(dir, 4) != 0) {
      rlang::abort(
        message = paste("You do not have read access to", dir),
        class = "dir_read_error"
      )
    }
  }
  options(wikimorphemes.dir = dir)

  return(dir)
}

.validate_cache_dir_write <- function(cache_dir) {
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

.get_latest_dump_date <- function() {
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
}

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

.download_latest_dump <- function() {
  dump_filename <- tempfile("wiktionary_dump", fileext = ".xml.bz2")
  download_check <- download.file(
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

.find_page_info <- function(dump_filename) {
  return(
    dplyr::mutate(
      tibble::tibble(
        start_line = grep("<page>", readLines(dump_filename))
      ),
      end_line = dplyr::lead(start_line - 1L),
      total_lines = end_line - start_line + 1L
    )
  )
}

.parse_dump <- function(dump_filename, page_info) {
  # Open a connec/tion. We'll continuously read from this until we reach the
  # end, parsing as we go.
  con <- file(dump_filename, open = "r")
  on.exit(close(con), add = TRUE)

  # There will be garbage at the start. Jump forward.
  if (page_info$start_line[[1]] > 1) {
    throwaway <- readLines(con, page_info$start_line[[1]] - 1L)
  }

  total_rows <- nrow(page_info)
  word_info <- vector(mode = "list", length = total_rows)

  # Now read page, which we can do simply by reading the number of lines in each
  # total_lines entry. This should be a for loop because we need each one to
  # read after the previous one, so the connection advances.
  for (i in seq_along(page_info$total_lines)) {
    if (i %% 10000 == 0) {
      cat(
        "Working on row",
        i,
        "of",
        total_rows,
        "\n"
      )
    }
    n_lines <- page_info$total_lines[[i]]

    # The last page doesn't have n_lines, so deal with that.
    if (is.na(n_lines)) {
      this_page_vector <- readLines(con, encoding = "UTF-8")
    } else {
      this_page_vector <- readLines(con, n_lines, encoding = "UTF-8")
    }

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
        this_entry <- .extract_relevant_english_wt(this_page)
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

  return(
    dplyr::as_tibble(
      dplyr::bind_rows(word_info)
    )
  )
}
