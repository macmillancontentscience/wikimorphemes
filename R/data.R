#' Sight Words
#'
#' A list of almost 1000 sight words. These are included here mainly to
#' override the Wiktionary breakdown of certain common words.
#'
#' The list was derived from the Fry sight words:
#' https://sightwords.com/sight-words/fry/#lists
#' Words with obvious breakdowns (e.g. "words" -> "word -s") were replaced with
#' the base word. A few cases of less-obvious breakdowns were similarly accepted
#' (e.g. "length" was excluded from the list to allow the breakdown "long -th").
#'
#' Contractions and words with three or fewer characters were left off the list,
#' since we don't break those down anyway.
#'
#' @format Character vector.
#' @source \url{https://sightwords.com/sight-words/fry/#lists}
"fry_sight_words"
