test_that("Wiktionary word list works.", {
  # Note: the first time you run this test, the Wiktionary word list will be
  # downloaded and cached.
  test_result <- wiktionary_word_list()
  expect_type(test_result, "character")
  expect_gte(length(test_result), 990000)
})
