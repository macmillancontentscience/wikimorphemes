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
      tidyr::unnest_wider(value) %>%
      dplyr::select(-name)
  )
}

#' Fetch the Content of a Wiktionary Word Page
#'
#' @param word The word to fetch.
#'
#' @return Character; the word's page, in wikitext format.
#' @keywords internal
.fetch_word <- function(word) {
  return(
    WikipediR::page_content(
      "en",
      "wiktionary",
      page_name = word,
      as_wikitext = TRUE,
      clean_response = TRUE
    )$wikitext$`*`
  )
}
