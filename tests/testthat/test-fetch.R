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
