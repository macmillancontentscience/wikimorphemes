
# misc imports ------------------------------------------------------------

#' @importFrom rlang .data
rlang::.data
#' @importFrom rlang .env
rlang::.env

#' @importFrom magrittr %>%
magrittr::`%>%`

# .list_pages_in_category -------------------------------------------------


#' List All Pages in a Category
#'
#' Wiktionary only returns 500 results at a time. Many categories have far more
#' than 500 results. Use this function to get them all.
#'
#' @inheritParams WikipediR::pages_in_category
#'
#' @return A tibble of results.
#' @keywords internal
.list_pages_in_category <- function(categories,
                                   type = c("page", "subcat", "file")) {
  initial <- WikipediR::pages_in_category(
    language = "en",
    project = "wiktionary",
    categories = categories,
    properties = c("id", "title"),
    type = type,
    limit = 500
  )
  this_df <- .clean_wiktionary_category_list(initial)
  continue <- initial$continue
  while(!is.null(continue)) {
    new_result <- WikipediR::pages_in_category(
      language = "en",
      project = "wiktionary",
      categories = categories,
      properties = c("id", "title"),
      type = type,
      limit = 500,
      extra_query = list(cmcontinue = continue$cmcontinue)
    )
    this_df <- dplyr::bind_rows(
      this_df,
      .clean_wiktionary_category_list(new_result)
    )
    continue <- new_result$continue
  }
  return(this_df)
}


# .clean_wiktionary_category_list -----------------------------------------


#' Get the Members of a Category Return
#'
#' @param query_return List; the return from
#'   \code{\link[WikipediR]{pages_in_category}}.
#'
#' @return A tibble of results.
#' @keywords internal
.clean_wiktionary_category_list <- function(query_return) {
  return(
    tibble::enframe(query_return$query$categorymembers) %>%
      tidyr::unnest_wider(.data$value) %>%
      dplyr::select(-.data$name)
  )
}


# .fetch_word -------------------------------------------------------------


#' Fetch the Content of a Wiktionary Word Page
#'
#' @param word The word to fetch.
#'
#' @return Character; the word's page, in wikitext format.
#' @keywords internal
.fetch_word <- function(word) {
  content <- tryCatch({
    WikipediR::page_content(
      "en",
      "wiktionary",
      page_name = word,
      as_wikitext = TRUE,
      clean_response = TRUE
    )$wikitext$`*`
  }, error = function(e) {
    return(character(0)) # decide on this
  })

  return(content)
}

# .fetch_english_word ---------------------------------------------------------

#' Fetch the English Section of a Wiktionary Word Page
#'
#' @inheritParams .fetch_word
#'
#' @return Character; the "English" section of the word's page, in wikitext
#'   format.
#' @keywords internal
.fetch_english_word <- function(word) {
  # Temporary hack to use wk download...
  if ("all_wiktionary_en" %in% ls(envir = .GlobalEnv)) { # nocov start
    # message("using wiktionary dump from global environment.")
    all_wiktionary_en <- get("all_wiktionary_en", envir = .GlobalEnv)
    content <- all_wiktionary_en %>%
      dplyr::filter(.data$title == .env$word) %>%
      dplyr::pull(.data$text)
    return(content)
  }   # nocov end
  message("hitting wiktionary API!")
  all_content <- .fetch_word(word)
  # Language sections are marked by "==<Language>==\n" headers.
  if (length(all_content) == 0) {
    # probably no page for this word
    return(character(0))
  }
  language_sections <- stringr::str_split(
    string = all_content,
    pattern = "(^|\\n)==(?!=)"
  )[[1]]
  english_section <- stringr::str_subset(
    string = language_sections,
    pattern = "^English==\\n"
  )
  return(english_section)
}

# .detect_irregular_wt ---------------------------------------------------------

#' Determine Membership in Irregular Category from Wikitext
#'
#' @param wt Character; wikitext resulting from a call to .fetch_english_word.
#'
#' @return TRUE if the word in question is in an irregular word category.
#' @keywords internal
.detect_irregular_wt <- function(wt) {
  # I think all irregular categories start with "[[Category:English irregular"
  # We likely will want to find specific categories later.
  irreg_patt <- "[[Category:English irregular"

  return(stringr::str_detect(string = wt, pattern = stringr::coll(irreg_patt)))
}

# .check_reconstructed_word -------------------------------------------------

#' Check that Word Breakdown is Close to Original Word
#'
#' @param original_word Character; the word before breakdown.
#' @param ... Character arguments that will be pasted together to reconstruct
#'   word.
#'
#' @return TRUE if the reconstructed word is "close enough" to the original.
#' @keywords internal
.check_reconstructed_word <- function(original_word, ...) {
  threshold <- 2 # seems right, so hard-coding for now
  # take out hyphens (maybe all non-alpha?)
  # https://github.com/jonthegeek/wikimorphemes/issues/8
  breakdown <- stringr::str_remove_all(c(...), "\\-")

  reconstructed_word <- paste0(breakdown, collapse = "")
  dist <- stringdist::stringdist(original_word, reconstructed_word)
  return(dist <= threshold)
}

# .check_nonexplosive_word -------------------------------------------------

#' Check that Word Breakdown doesn't Contain Original Word
#'
#' @param original_word Character; the word before breakdown.
#' @param ... Character pieces of the word after breakdown.
#'
#' @return TRUE if the word breakdown does not contain the original word as a
#'   piece.
#' @keywords internal
.check_nonexplosive_word <- function(original_word, ...) {
  breakdown <- list(...)
  # not sure whether I need to allow for a single word piece that is the
  # original word. We can probably reject that, too.
  if (original_word %in% breakdown ) {
    return(FALSE)
  }
  return(TRUE)
}


# .make_template_pattern -------------------------------------------------

#' Construct Regex for Wiktionary Templates
#'
#' Currently works for templates of the "affix" type.
#'
#' @param type Character; specifies which template to construct.
#'
#' @return regular expression for detecting the given type.
#' @keywords internal
.make_template_pattern <- function(type) {
  return(paste0("\\{\\{", type, "\\|en\\|([^}]+)\\}\\}"))
}
