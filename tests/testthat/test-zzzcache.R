# These tests mess with the cache, so I moved them to the end to try to unbreak
# some coverage.

test_that("non-cache works as expected", {
  temp_dir <- tempdir()
  test_result <- .cache_wikitext(cache_dir = temp_dir)
  expect_null(test_result)

  memoise::drop_cache(.cache_wikitext)(cache_dir = temp_dir)
  options(wikimorphemes.dir = NULL)

  cache_file <- fs::path(
    temp_dir,
    "wikitext_en",
    ext = "rds"
  )
  saveRDS(
    mtcars,
    cache_file
  )

  test_result <- .cache_wikitext(cache_dir = temp_dir)
  expect_identical(
    test_result,
    mtcars
  )
  options(wikimorphemes.dir = NULL)
})
