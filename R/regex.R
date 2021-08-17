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

# nocov start

#' Helper to Generate Regex Positive Look Behind
#'
#' @param string Character; a string or strings to modify with regex. The thing
#'   to look for behind the "main" regex.
#'
#' @return A modified string.
#' @keywords internal
.regex_positive_lookbehind <- function(string) {
  paste0(
    "(?<=",
    string,
    ")"
  )
}

#' Helper to Generate Regex Positive Look Ahead
#'
#' @param string Character; a string or strings to modify with regex. The thing
#'   to look for after the "main" regex.
#'
#' @return A modified string.
#' @keywords internal
.regex_positive_lookahead <- function(string) {
  paste0(
    "(?=",
    string,
    ")"
  )
}

#' Helper to Generate Regex Negative Look Ahead
#'
#' @param string Character; a string or strings to modify with regex. The thing
#'   to look for (and make sure it doesn't occur) after the "main" regex.
#'
#' @return A modified string.
#' @keywords internal
.regex_negative_lookahead <- function(string) {
  paste0(
    "(?!",
    string,
    ")"
  )
}

#' Helper to Generate Regex For Character Exceptions
#'
#' @param string Character; a string or strings to modify with regex. The thing
#'   to not allow in the regex.
#'
#' @return A modified string.
#' @keywords internal
.regex_any_character_except <- function(string) {
  paste0(
    "[^",
    string,
    "]"
  )
}

#' Helper to Generate Regex Or
#'
#' @param string Character; strings to concatenate.
#'
#' @return A length-1 string.
#' @keywords internal
.regex_or <- function(strings) {
  paste0(
    "(",
    paste(strings, collapse = "|"),
    ")"
  )
}

#' Helper to Generate Regex One or More
#'
#' @param string Character; a string or strings to modify with regex. The thing
#'   to check for at least one of.
#'
#' @return A modified string.
#' @keywords internal
.regex_one_or_more <- function(string) {
  paste0(string, "+")
}

#' Helper to Generate Regex Zero or More
#'
#' @param string Character; a string or strings to modify with regex. The thing
#'   to allow to exist.
#'
#' @return A modified string.
#' @keywords internal
.regex_zero_or_more <- function(string) {
  paste0(string, "*")
}

# nocov end
