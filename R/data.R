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

#' Sight Words
#'
#' A list of almost 1000 sight words. These are included here mainly to override
#' the Wiktionary breakdown of certain common words.
#'
#' The list was derived from the Fry sight words:
#' https://sightwords.com/sight-words/fry/#lists Words with obvious breakdowns
#' (e.g. "words" -> "word -s") were replaced with the base word. A few cases of
#' less-obvious breakdowns were similarly accepted (e.g. "length" was excluded
#' from the list to allow the breakdown "long -th").
#'
#' Contractions and words with three or fewer characters were left off the list,
#' since we don't break those down anyway.
#'
#' @format Character vector.
#' @source \url{https://sightwords.com/sight-words/fry/#lists}
#' @return A character vector of words that should not be split into morphemes.
#' @export
default_sight_words <- function() {
  return(.fry_sight_words)
}
