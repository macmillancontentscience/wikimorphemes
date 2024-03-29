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

#' Remove Hyphens from Affixes
#'
#' For some applications, it's convenient to remove hyphens from morpheme
#' pieces. That's more tedious than it should be.
#'
#' @param processed_word A word processed into pieces by
#'   \code{\link{process_word}}.
#'
#' @return The processed word without hyphens.
#' @export
#'
#' @examples
#' processed_word <- process_word("Christmas")
#' processed_word
#' remove_hyphens(processed_word)
remove_hyphens <- function(processed_word) {
  processed_word_2 <- stringr::str_remove_all(processed_word, "\\-")
  names(processed_word_2) <- names(processed_word)
  return(processed_word_2)
}

#' Correct Hyphens in Processed Words
#'
#' Sometimes we gain or lose hyphens in various processes. Standardize them.
#'
#' @param processed_word Character vector; the breakdown of the word.
#'
#' @return The processed words with proper hyphens.
#' @keywords internal
.fix_hyphens <- function(processed_word) {
  processed_word[names(processed_word) == .prefix_name] <- paste0(
    processed_word[names(processed_word) == .prefix_name], "-"
  )
  processed_word[names(processed_word) == .suffix_name] <- paste0(
    "-", processed_word[names(processed_word) == .suffix_name]
  )
  processed_word[names(processed_word) == .interfix_name] <- paste0(
    "-", processed_word[names(processed_word) == .interfix_name], "-"
  )
  processed_word[names(processed_word) == .inflection_name] <- paste0(
    "-", processed_word[names(processed_word) == .inflection_name]
  )

  return(
    rlang::set_names(
      stringr::str_replace_all(processed_word, "--", "-"),
      names(processed_word)
    )
  )
}
