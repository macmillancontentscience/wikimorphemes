test_that("split_inflections works", {

  testthat::expect_identical(
    split_inflections("lighter"),
    c(base_word = "light", ending = "er")
  )

  testthat::expect_identical(
    split_inflections("lightest"),
    c(base_word = "light", ending = "est")
  )

  testthat::expect_identical(
    split_inflections("lighting"),
    c(base_word = "light", ending = "ing")
  )

  testthat::expect_identical(
    split_inflections("lights"),
    c(base_word = "light", ending = "s")
  )

  testthat::expect_identical(
    split_inflections("running"),
    c(base_word = "run", ending = "ing")
  )

  testthat::expect_identical(
    split_inflections("scrapped"),
    c(base_word = "scrap", ending = "ed")
  )

  testthat::expect_identical(
    split_inflections("scraped"),
    c(base_word = "scrape", ending = "ed")
  )

  testthat::expect_identical(
    split_inflections("escaping"),
    c(base_word = "escape", ending = "ing")
  )

  # non-splittable words come back unchanged
  testthat::expect_identical(
    split_inflections("escape"),
    "escape"
  )

  # non-English words come back empty
  testthat::expect_identical(
    split_inflections("bueno"),
    character(0)
  )

  # irregular words come back unchanged
  testthat::expect_identical(
    split_inflections("ground"),
    "ground"
  )

  # unclassified irregulars are caught by stringdist check.
  testthat::expect_identical(
    split_inflections("best"),
    "best"
  )

})
