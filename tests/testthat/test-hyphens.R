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

test_that("Can remove hyphens", {
  processed_word <- process_word("Christmas")
  expect_identical(processed_word, c(base_word = "Christ", suffix = "-mas"))
  test_result <- remove_hyphens(processed_word)
  expect_identical(test_result, c(base_word = "Christ", suffix = "mas"))
})
