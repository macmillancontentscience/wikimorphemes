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
    process_word("upendings", max_lookup_age_days = 0),
    c(prefix = "up-", base_word = "end", inflection = "-ing", inflection = "-s")
  )

  testthat::expect_identical(
    process_word("disestablishmentarianism", max_lookup_age_days = 0),
    c(
      prefix = "dis-", base_word = "establish",
      suffix = "-ment", suffix = "-arian", suffix = "-ism"
    )
  )

  testthat::expect_identical(
    process_word("unaffable", max_lookup_age_days = 0),
    c(prefix = "un-", base_word = "affable")
  )

  testthat::expect_identical(
    process_word("pesticides", max_lookup_age_days = 0),
    c(base_word = "pest", interfix = "-i-", suffix = "-cide", inflection = "-s")
  )

  testthat::expect_identical(
    process_word("neurogenic", max_lookup_age_days = 0),
    # Think about breakdown of "genic" into "gene ic". Genic was aready marked
    # as a suffix; should it be broken further? For now, "-genic" is *not* split
    # further. Note that this is how it is on wiktionary, not something *we*
    # decide.
    c(prefix = "neuro-", suffix = "-genic")
  )

  testthat::expect_identical(
    process_word("bedewed", max_lookup_age_days = 0),
    c(prefix = "be-", base_word = "dew", inflection = "-ed")
  )

  testthat::expect_identical(
    process_word("rainbow", max_lookup_age_days = 0),
    c(base_word = "rain", base_word = "bow")
  )

  testthat::expect_identical(
    process_word("clearinghouse", max_lookup_age_days = 0),
    c(base_word = "clear", inflection = "-ing", base_word = "house")
  )

  testthat::expect_identical(
    process_word("passersby", max_lookup_age_days = 0),
    c(base_word = "pass", suffix = "-er", base_word = "by", inflection = "-s")
  )

  # DON'T process "-mas" into "ma s"
  testthat::expect_identical(
    process_word("Christmas", max_lookup_age_days = 0),
    c(base_word = "Christ", suffix = "-mas")
  )

  testthat::expect_identical(
    process_word("every", max_lookup_age_days = 0),
    c(base_word = "every")
  )

  testthat::expect_identical(
    process_word("lenses", max_lookup_age_days = 0),
    c(base_word = "lens", inflection = "-s")
  )

  # check recursion limit
  testthat::expect_message(
    .process_word_recursive("lovingly", max_depth = 1, max_lookup_age_days = 0),
    "maximum recursion depth of 1 reached"
  )
})

test_that("lookup corner cases work.", {
  expect_null("I don't test where we have a lookup but it's too old.")

  # Before saving the fake lookup, make sure a broken cache properly returns as
  # NULL.
  old_option <- getOption("wikimorphemes.dir")

  test_result <- .cache_lookup(cache_dir = tempdir())
  expect_null(test_result)

  memoise::drop_cache(.cache_lookup)(cache_dir = tempdir())
  options(wikimorphemes.dir = old_option)

  fake_lookup <- dplyr::tibble(
    word = "upendings",
    morphemes = list(
      c(prefix = "down", base_word = "start")
    ),
    n_morphemes = 2L,
    timestamp = lubridate::now()
  )

  saveRDS(
    fake_lookup,
    fs::path(
      tempdir(),
      "wikimorphemes",
      ext = "rds"
    )
  )

  testthat::expect_identical(
    process_word(word = "upendings", cache_dir = tempdir()),
    c(prefix = "down", base_word = "start")
  )
  memoise::drop_cache(.cache_lookup)(cache_dir = tempdir())
  options(wikimorphemes.dir = old_option)
})
