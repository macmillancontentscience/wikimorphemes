# These tests mess with the cache, so I moved them to the end to try to unbreak
# some coverage.

test_that("non-cache works as expected", {
  old_cachedir <- getOption("wikimorphemes.dir")
  on.exit({
    options(wikimorphemes.dir = old_cachedir)
    memoise::forget(.cache_wikitext)
  })
  set_wikimorphemes_cache_dir(tempdir())
  memoise::forget(.cache_wikitext)
  test_result <- .cache_wikitext()
  expect_null(test_result)

  cache_file <- fs::path(
    tempdir(),
    "wikitext_en",
    ext = "rds"
  )
  saveRDS(
    mtcars,
    cache_file
  )
  on.exit(
    unlink(cache_file),
    add = TRUE
  )
  memoise::forget(.cache_wikitext)

  test_result <- .cache_wikitext()
  expect_identical(
    test_result,
    mtcars
  )
})
