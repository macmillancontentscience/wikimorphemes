test_that(".split_inflections works", {
  # Set up a function so we don't have to fetch each time, now that the fetch
  # happens outside of the .split_ functions. I'm starting it with .test_ to
  # avoid any possible collisions.
  .test_fetch_then_split <- function(word) {
    english_content <- .fetch_english_word(word)
    return(.split_inflections(english_content, word))
  }

  testthat::expect_identical(
    .test_fetch_then_split("lighter"),
    c(base_word = "light", inflection = "-er")
  )

  testthat::expect_identical(
    .test_fetch_then_split("lightest"),
    c(base_word = "light", inflection = "-est")
  )

  testthat::expect_identical(
    .test_fetch_then_split("lighting"),
    c(base_word = "light", inflection = "-ing")
  )

  testthat::expect_identical(
    .test_fetch_then_split("lights"),
    c(base_word = "light", inflection = "-s")
  )

  testthat::expect_identical(
    .test_fetch_then_split("running"),
    c(base_word = "run", inflection = "-ing")
  )

  testthat::expect_identical(
    .test_fetch_then_split("scrapped"),
    c(base_word = "scrap", inflection = "-ed")
  )

  testthat::expect_identical(
    .test_fetch_then_split("scraped"),
    c(base_word = "scrape", inflection = "-ed")
  )

  testthat::expect_identical(
    .test_fetch_then_split("escaping"),
    c(base_word = "escape", inflection = "-ing")
  )

  # non-splittable words come back unchanged
  testthat::expect_identical(
    .test_fetch_then_split("escape"),
    "escape"
  )

  # non-English words come back unchanged, too
  testthat::expect_identical(
    .test_fetch_then_split("bueno"),
    "bueno"
  )

  # irregular words come back unchanged
  testthat::expect_identical(
    .test_fetch_then_split("ground"),
    "ground"
  )

  # unclassified irregulars are caught by stringdist check.
  testthat::expect_identical(
    .test_fetch_then_split("best"),
    "best"
  )
})

test_that("process_word works", {
  testthat::expect_identical(
    process_word("upendings", use_lookup = FALSE),
    c(prefix = "up-", base_word = "end", inflection = "-ing", inflection = "-s")
  )

  testthat::expect_identical(
    process_word("disestablishmentarianism", use_lookup = FALSE),
    c(
      prefix = "dis-", base_word = "establish",
      suffix = "-ment", suffix = "-arian", suffix = "-ism"
    )
  )

  testthat::expect_identical(
    process_word("unaffable", use_lookup = FALSE),
    c(prefix = "un-", base_word = "affable")
  )

  testthat::expect_identical(
    process_word("pesticides", use_lookup = FALSE),
    c(base_word = "pest", interfix = "-i-", suffix = "-cide", inflection = "-s")
  )

  testthat::expect_identical(
    process_word("neurogenic", use_lookup = FALSE),
    # Think about breakdown of "genic" into "gene ic". Genic was aready marked
    # as a suffix; should it be broken further? For now, "-genic" is *not* split
    # further. Note that this is how it is on wiktionary, not something *we*
    # decide.
    c(prefix = "neuro-", suffix = "-genic")
  )

  testthat::expect_identical(
    process_word("bedewed", use_lookup = FALSE),
    c(prefix = "be-", base_word = "dew", inflection = "-ed")
  )

  testthat::expect_identical(
    process_word("rainbow", use_lookup = FALSE),
    c(base_word = "rain", base_word = "bow")
  )

  testthat::expect_identical(
    process_word("clearinghouse", use_lookup = FALSE),
    c(base_word = "clear", inflection = "-ing", base_word = "house")
  )

  # We now require inflected words to actually end with their inflection, but
  # make an exception to accomodate weird plurals like "passersby"
  testthat::expect_identical(
    process_word("passersby", use_lookup = FALSE),
    c(base_word = "pass", suffix = "-er", base_word = "by", inflection = "-s")
  )

  # DON'T process "-mas" into "ma s"
  testthat::expect_identical(
    process_word("Christmas", use_lookup = FALSE),
    c(base_word = "Christ", suffix = "-mas")
  )

  testthat::expect_identical(
    process_word("every", use_lookup = FALSE),
    c(base_word = "every")
  )

  testthat::expect_identical(
    process_word("lenses", use_lookup = FALSE),
    c(base_word = "lens", inflection = "-s")
  )

  # check recursion limit
  testthat::expect_message(
    .process_word_recursive("lovingly", max_depth = 1, use_lookup = FALSE),
    "maximum recursion depth of 1 reached"
  )

  # The sight words list changes some things...
  testthat::expect_identical(
    process_word(
      "into",
      sight_words = default_sight_words(),
      use_lookup = FALSE
    ),
    c(base_word = "into") # without sight words: "in" + "to"
  )
})

test_that("process_word deals with fake words gracefully.", {
  word <- "sdjklf"
  test_result <- process_word(word, use_lookup = FALSE)
  expect_identical(test_result, rlang::set_names(word, "base_word"))
  test_result <- process_word(word, use_lookup = TRUE)
  expect_identical(test_result, rlang::set_names(word, "base_word"))
})

test_that("Can use a lookup other than the default.", {
  # Before saving the fake lookup, make sure a broken cache properly returns as
  # NULL.
  old_option <- getOption("wikimorphemes.dir")
  on.exit(
    options(wikimorphemes.dir = old_option)
  )
  set_wikimorphemes_cache_dir(tempdir())

  memoise::forget(.cache_lookup)
  reset_lookup()
  test_result <- .cache_lookup()
  expect_null(test_result)

  fake_lookup <- dplyr::tibble(
    word = "upendings",
    morphemes = list(
      c(prefix = "down", base_word = "start")
    ),
    n_morphemes = 2L,
    timestamp = lubridate::now()
  )

  filename <- fs::path(
    tempdir(),
    "wikimorphemes_lookup",
    ext = "rds"
  )

  saveRDS(
    fake_lookup,
    filename
  )
  on.exit(
    {
      unlink(filename)
      memoise::forget(.cache_lookup)
    },
    add = TRUE
  )

  # Now we need to force a reload of the cache.
  memoise::forget(.cache_lookup)
  memoise::forget(.populate_env_lookup)

  testthat::expect_identical(
    process_word(word = "upendings"),
    c(prefix = "down", base_word = "start")
  )
})
