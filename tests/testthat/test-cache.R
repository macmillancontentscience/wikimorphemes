test_that("Wiktionary word list works.", {
  # Note: the first time you run this test, the Wiktionary word list will be
  # downloaded and cached.
  test_result <- wiktionary_word_list()
  expect_type(test_result, "character")
  expect_gte(length(test_result), 1000000)
})

test_that("The cache filename generator respects filenames at least.", {
  test_result <- .generate_cache_write_filename("testing")
  expect_true(
    stringr::str_ends(test_result, "rds")
  )

  test_result <- .generate_cache_write_filename("testing", "csv")
  expect_true(
    stringr::str_ends(test_result, "csv")
  )
})
