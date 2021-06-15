#' Remove Hyphens from Affixes
#'
#' For some applications, it's convenient to remove hyphens from morpheme
#' pieces. That's more tedious than it should be.
#'
#' @param processed_word A word processed into pieces by \code{\link{process_word}}.
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
