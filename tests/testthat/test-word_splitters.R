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


test_that("process_word works", {
  testthat::expect_identical(
    process_word("upendings"),
    c(prefix = "up", base_word = "end", inflection = "ing", inflection = "s")
  )

  testthat::expect_identical(
    process_word("disestablishmentarianism"),
    c(prefix = "dis", base_word = "establish",
      suffix = "ment", suffix = "arian", suffix = "ism")
  )

  testthat::expect_identical(
    process_word("unaffable"),
    c(prefix = "un", base_word = "affable")
  )

  testthat::expect_identical(
    process_word("pesticides"),
    c(base_word = "pest", interfix = "i", suffix = "cide", inflection = "s")
  )

  testthat::expect_identical(
    process_word("neurogenic"),
    # Think about breakdown of "genic" into "gene ic". Genic was aready marked
    # as a suffix; should it be broken further? For now, "-genic" is *not* split
    # further.
    c(prefix = "neuro", suffix = "genic")
  )

  testthat::expect_identical(
    process_word("bedewed"),
    c(prefix = "be", base_word = "dew", inflection = "ed")
  )

  testthat::expect_identical(
    process_word("rainbow"),
    c(base_word = "rain", base_word = "bow")
  )

  testthat::expect_identical(
    process_word("clearinghouse"),
    c(base_word = "clear", inflection = "ing", base_word = "house")
  )

  testthat::expect_identical(
    process_word("passersby"),
    c(base_word = "pass", suffix = "er", base_word = "by", inflection = "s")
  )

  # DON'T process "-mas" into "ma s"
  testthat::expect_identical(
    process_word("Christmas"),
    c(base_word = "Christ", suffix = "mas")
  )

  testthat::expect_identical(
    process_word("every"),
    c(base_word = "every")
  )

  testthat::expect_identical(
    process_word("lenses"),
    c(base_word = "lens", inflection = "s")
  )

  # check recursion limit
  testthat::expect_message(
    process_word_recursive("lovingly", max_depth = 1),
    "maximum recursion depth of 1 reached"
  )
})

