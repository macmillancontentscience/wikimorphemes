test_that("Can remove hyphens", {
  processed_word <- process_word("Christmas")
  expect_identical(processed_word, c(base_word = "Christ", suffix = "-mas"))
  test_result <- remove_hyphens(processed_word)
  expect_identical(test_result, c(base_word = "Christ", suffix = "mas"))
})
