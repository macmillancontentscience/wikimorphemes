.wikimorphemes_env <- new.env()
assign(
  "lookup",
  # By default, the lookup is empty to begin with. Various calls will populate
  # this with real data.
  tibble::tibble(
    word = character(0),
    morphemes = vector(mode = "list", length = 0),
    n_morphemes = integer(0),
    # This appears to be the easiest way to create a length-0 datetime.
    timestamp = Sys.time()[0]
  ),
  envir = .wikimorphemes_env
)


.populate_env_lookup <- function(cache_dir = wikimorphemes_cache_dir()) {
  cached_lookup <- .cache_lookup(cache_dir)
  if (!is.null(cached_lookup)) {
    if (nrow(.wikimorphemes_env$lookup)) {
      # It might be faster to only sort out cases that overlap, rather than
      # arranging the entire lookup... but it should be super rare that they
      # both have something in the environment lookup AND have a cache (they
      # have to have changed cache locations after doing a lookup), so I'm not
      # going to take the time to optimize this.
      .wikimorphemes_env$lookup <- dplyr::bind_rows(
        .wikimorphemes_env$lookup,
        cached_lookup
      ) %>%
        dplyr::arrange(word, dplyr::desc(timestamp)) %>%
        dplyr::distinct(word, .keep_all = TRUE)
    } else {
      .wikimorphemes_env$lookup <- cached_lookup
    }
  }

  return(invisible(NULL))
}

.update_env_lookup <- function(word, morphemes) {
  .wikimorphemes_env$lookup[.wikimorphemes_env$lookup$word == word] <- NULL
  .wikimorphemes_env$lookup <- dplyr::bind_rows(
    .wikimorphemes_env$lookup,
    tibble::tibble(
      word = word,
      morphemes = morphemes,
      n_morphemes = length(morphemes),
      # This appears to be the easiest way to create a length-0 datetime.
      timestamp = lubridate::now()
    )
  )
}
