
# split_inflections ------------------------------------------------------


#' Split Standard Verb, Noun, and Adjective Endings of a Word
#'
#' @param word Character; a word to process.
#'
#' @return Character; the word with standard endings split off.
#' @export
#' @examples
split_inflections <- function(word) {
  english_content <- .fetch_english_word(word)
  if (length(english_content) == 0) {
    # not an english word
    return(character(0)) # or NA_character?
  }

  if (.detect_irregular_wt(english_content)) {
    # for now, return irregular words without attempting to split inflections.
    return(word)
  }

  # Wiktionary has a variety of templates for various standard endings. In some
  # cases there are multiple ways to denote word structure. Maybe later make
  # utility function to maintain these?
  patterns_endings <- c(
    # <pattern_to_detect> = <standard_ending>
    "\\{\\{plural of\\|en\\|([^}]+)\\}\\}" = "s",
    "\\{\\{en-third-person singular of\\|([^\\|\\}]+)" = "s",
    "\\{\\{en-ing form of\\|([^\\|\\}]+)" = "ing",  # "escaping"
    "\\{\\{present participle of\\|en\\|([^\\|\\}]+)" = "ing",
    # I don't think this is a very general pattern. Maybe replace with
    # {{inflection of| }} template?
    "The action of the verb '''to \\[\\[([^\\]]+)\\]\\]'''" = "ing",
    "\\{\\{past participle of\\|en\\|([^\\|\\}]+)" = "ed",
    "\\{\\{en-past of\\|([^\\|\\}]+)" = "ed",
    "\\{\\{en-comparative of\\|([^\\|\\}]+)" = "er",
    "\\{\\{en-superlative of\\|([^\\|\\}]+)" = "est"
  )

  candidate_breakdowns <- vector(mode = "list")
  for (patt in names(patterns_endings)) {
    ending <- patterns_endings[[patt]]
    base_word <- stringr::str_match(english_content, patt)[[2]]
    if (!is.na(base_word)) {
      if (.check_reconstructed_word(word, base_word, ending)) {
        breakdown <- c("base_word" = base_word, "ending" = ending)
        candidate_breakdowns[[length(candidate_breakdowns)+1]] <- breakdown
      }
    }
  }
  if (length(candidate_breakdowns) == 0) {
    return(word)
  }
  unique_breakdowns <- unique(candidate_breakdowns)
  if (length(unique_breakdowns) > 1) {
    warning("more than one unique breakdown found for: ", word) # nocov
  }
  return(unique_breakdowns[[1]])
}

