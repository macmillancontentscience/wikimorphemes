test_that("we can retrive wikitionary data", {
  english <- .fetch_english_word("utmost")
  testthat::expect_match(english, regexp = "^English")

  english <- .fetch_english_word("apple")
  testthat::expect_match(english, regexp = "^English")

  not_english <- .fetch_english_word("bueno")
  testthat::expect_identical(not_english, character(0))

  page_list <- .list_pages_in_category(
    categories = "English_words_suffixed_with_-ability",
    type = "page"
  )
  testthat::expect_s3_class(page_list, "tbl_df")
  testthat::expect_gte(nrow(page_list), 800)
  testthat::expect_equal(length(page_list), 3)
})


