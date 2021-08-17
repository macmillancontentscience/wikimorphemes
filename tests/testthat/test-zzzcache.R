# Copyright 2021 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
