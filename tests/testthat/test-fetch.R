test_that("we can retrieve wikitionary data", {
  english <- .fetch_english_word("utmost")
  testthat::expect_match(english, regexp = "^\\{\\{was wotd")

  english <- .fetch_english_word("apple")
  testthat::expect_match(english, regexp = "^\\{\\{wikipedia\\}\\}")

  not_english <- .fetch_english_word("bueno")
  testthat::expect_identical(not_english, character(0))
})

test_that("We can pull relevant bits out of wikitext.", {
  # Multiple etymologies: number, ground, wind, found
  test_result <- .fetch_english_word("number")
  expect_lte(nchar(test_result), 10000)
  expect_false(stringr::str_detect(test_result, "Etymology 2"))

  # 1 etymology: numb
  test_result <- .fetch_english_word("numb")
  expect_lte(nchar(test_result), 6000)

  # 0 etymologies: founds
  test_result <- .fetch_english_word("founds")
  expect_lte(nchar(test_result), 500)
})

test_that("Corner cases work.", {
  expect_false(.check_nonexplosive_word("word", "word"))
})
