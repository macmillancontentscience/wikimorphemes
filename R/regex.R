.regex_positive_lookbehind <- function(string) {
  paste0(
    "(?<=",
    string,
    ")"
  )
}

.regex_positive_lookahead <- function(string) {
  paste0(
    "(?=",
    string,
    ")"
  )
}

.regex_negative_lookahead <- function(string) {
  paste0(
    "(?!",
    string,
    ")"
  )
}

.regex_any_character_except <- function(string) {
  paste0(
    "[^",
    string,
    "]"
  )
}

.regex_or <- function(strings) {
  paste0(
    "(",
    paste(strings, collapse = "|"),
    ")"
  )
}

.regex_one_or_more <- function(string) {
  paste0(string, "+")
}

.regex_zero_or_more <- function(string) {
  paste0(string, "*")
}
