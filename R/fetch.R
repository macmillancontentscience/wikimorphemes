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

# .fetch_word -------------------------------------------------------------

#' Fetch the Content of a Wiktionary Word Page
#'
#' @param word The word to fetch.
#'
#' @return Character; the word's page, in wikitext format.
#' @keywords internal
.fetch_word <- function(word) {
  content <- tryCatch( # nocov start
    {
      WikipediR::page_content(
        "en",
        "wiktionary",
        page_name = word,
        as_wikitext = TRUE,
        clean_response = TRUE
      )$wikitext$`*`
    },
    error = function(e) {
      return(character(0)) # decide on this
    }
  )

  return(content) # nocov end
}

# .fetch_english_word ---------------------------------------------------------

#' Fetch the English Section of a Wiktionary Word Page
#'
#' @inheritParams .fetch_word
#'
#' @return Character; the "English" section of the word's page, in wikitext
#'   format. If there isn't an "English" section but there's a "Translingual"
#'   section, that will be returned instead.
#' @keywords internal
.fetch_english_word <- function(word) {
  # Use the cache if they have it.
  if (!is.null(.cache_wikitext())) {
    content <- .cache_wikitext() %>% # nocov start
      dplyr::filter(.data$word == .env$word) %>%
      dplyr::pull(.data$wikitext)
    return(content) # nocov end
  }
  message("hitting wiktionary API!") # nocov start
  all_content <- .fetch_word(word)

  if (length(all_content) == 0) {
    # probably no page for this word
    return(character(0))
  }

  # If there's a redirect, get that.
  if (stringr::str_starts(all_content, stringr::fixed("#REDIRECT"))) {
    all_content <- .fetch_redirect(all_content)
  }
  # It can be down to length 0 again at this point if the redirect is bad.
  if (length(all_content) == 0) {
    # probably no page for this word
    return(character(0))
  }

  return(.extract_relevant_english_wt(all_content)) # nocov end
}

#' Fetch the Redirect Content for a Word
#'
#' Some words don't technically have their own entry, but instead are a redirect
#' to another word.
#'
#' @param wt Wikitext that begins with "#REDIRECT".
#'
#' @return The wt of the target word.
#' @keywords internal
.fetch_redirect <- function(wt) {
  word <- stringr::str_match(
    wt,
    "#REDIRECT \\[\\[([^]]+)"
  )[[2]]
  return(.fetch_word(word))
}

#' Extract the Relevant English Sections of a Wikitext Entry
#'
#' Pull just the English section out of a wikitext entry and drop things like
#' extra etymologies or anagrams.
#'
#' @inheritParams .extract_english_wt
#'
#' @return The relevant sections from the English portion of the wikitext.
#' @keywords internal
.extract_relevant_english_wt <- function(wt) {
  english_section <- .extract_english_wt(wt) # nocov start
  if (length(english_section)) {
    return(.drop_irrelevant_sections(english_section))
  } else {
    return(character(0))
  } # nocov end
}

#' Split Wikitext into Sections
#'
#' @param wt The word's page, in wikitext format (returned by
#'   \code{\link{.fetch_word}}), or a subsection thereof.
#' @param depth Integer; the heading depth (the number of ='s before the
#'   heading).
#' @param keep_first Whether to keep front matter before the first section.
#'
#' @return A named character vector of the sections.
#' @keywords internal
.split_sections <- function(wt, depth, keep_first = TRUE) {
  heading_indicator_piece <- "=" # nocov start
  heading_indicator <- paste(
    rep(heading_indicator_piece, times = depth),
    collapse = ""
  )
  maybe_extra_heading_indicators <- .regex_zero_or_more(heading_indicator_piece)
  normal_text <- .regex_any_character_except(heading_indicator_piece)
  maybe_normal_text <- .regex_zero_or_more(normal_text)
  heading_text <- .regex_one_or_more(normal_text)
  beginning <- .regex_or(c("^", "\n", ">"))

  heading_names <- stringr::str_extract_all(
    string = wt,
    pattern = paste0(
      .regex_positive_lookbehind(
        paste0(beginning, heading_indicator)
      ),
      heading_text,
      .regex_positive_lookahead(
        paste0(
          heading_indicator,
          maybe_extra_heading_indicators,
          maybe_normal_text
        )
      )
    )
  )[[1]]
  if (length(heading_names)) {
    sections <- stringr::str_split(
      string = wt,
      pattern = paste0(
        beginning,
        heading_indicator,
        heading_text,
        heading_indicator,
        maybe_extra_heading_indicators,
        .regex_negative_lookahead(heading_indicator_piece)
      )
    )[[1]]

    # If we're keeping the first section, we need to create a heading for it.
    # Otherwise drop it and use the existing names.
    if (keep_first & nchar(sections[[1]])) {
      heading_names <- c(
        "Front matter",
        heading_names
      )
    } else {
      sections <- sections[-1]
    }

    # Get rid of extra \n's.
    sections <- stringr::str_trim(sections)
    sections <- stringr::str_replace_all(
      sections,
      .regex_one_or_more("\n"),
      "\n"
    )

    if (length(heading_names) != length(sections)) {
      rlang::abort(
        message = paste(
          "Section length mismatch. These are the section names:",
          paste(heading_names, collapse = ", "),
          "And this is the text minus those section breaks:",
          paste(sections, collapses = "\n"),
          sep = "\n"
        )
      )
    }

    names(sections) <- heading_names

    return(sections)
  } else {
    return(character(0))
  } # nocov end
}

#' Recombine Sections into Wikitext
#'
#' @param sections A named character vector of sections returned by
#'   \code{\link{.split_sections}}.
#' @param depth The depth at which these sections were extracted.
#'
#' @return A character scalar of the recombined sections.
#' @keywords internal
.recombine_sections <- function(sections, depth) {
  heading_indicator_piece <- "=" # nocov start
  heading_indicator <- paste(
    rep(heading_indicator_piece, times = depth),
    collapse = ""
  )
  section_names <- paste0(
    heading_indicator,
    names(sections),
    heading_indicator
  )
  section_names <- ifelse(
    names(sections) == "Front matter",
    "",
    section_names
  )
  collapsed_text <- stringr::str_trim(
    paste(
      section_names,
      sections,
      sep = "\n",
      collapse = "\n"
    )
  )

  return(collapsed_text) # nocov end
}

#' Get Rid of Chaff in Wikitext
#'
#' @inheritParams .split_sections
#'
#' @return A character scalar of the cleaned wikitext.
#' @keywords internal
.drop_irrelevant_sections <- function(wt, depth = 3L) {
  # General strategy: divide it up at depth, then call this for all of those
  # with depth + 1. If there aren't any headings at this level, we're done.
  sections <- .split_sections(wt, depth) # nocov start
  if (length(sections)) {
    # For each element of sections, call this with depth + 1.
    sections <- purrr::map_chr(
      sections,
      .drop_irrelevant_sections,
      depth = depth + 1L
    )

    # Remove secondary etymologies and irrelevant sections at this level.
    extra_etymologies <- stringr::str_subset(
      unique(names(sections)),
      "Etymology (\\d{2,})|[02-9]"
    )
    irrelevant_headings <- c(
      "Anagrams",
      "Antonyms",
      "Derived terms",
      "Descendants",
      "Hyponyms",
      "Pronunciation",
      "References",
      "Related terms",
      "See also",
      "Synonyms",
      "Translations",
      extra_etymologies
    )
    sections <- sections[!(names(sections) %in% irrelevant_headings)]

    # Recombine and return.
    sections <- .recombine_sections(sections, depth)

    return(sections)
  } else {
    return(wt)
  } # nocov end
}

#' Extract the English Section of a Wikitext Entry
#'
#' Pull just the English section out of a wikitext entry.
#'
#' @param wt The word's page, in wikitext format (returned by
#'   \code{\link{.fetch_word}}).
#'
#' @return The English (or Translingual) portion of the wikitext, minus
#'   irrelevant sections.
#' @keywords internal
.extract_english_wt <- function(wt) {
  # Language sections are always depth = 2 ("==Language==")
  # nocov start
  language_sections <- .split_sections(wt = wt, depth = 2, keep_first = FALSE)
  english_section <- unname(
    language_sections[names(language_sections) == "English"]
  )
  # If there isn't an English section, check for translingual.
  if (!length(english_section)) {
    english_section <- unname(
      language_sections[names(language_sections) == "Translingual"]
    )
  }
  return(english_section) # nocov end
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
  # https://github.com/macmillancontentscience/wikimorphemes/issues/8
  # ... take out from *both* sides of equation to do fair check.
  breakdown <- stringr::str_remove_all(c(...), "\\-")
  original_word <- stringr::str_remove_all(original_word, "\\-")

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
  breakdown <- unlist(list(...))
  # We previously looked at whether the original word was anywhere inside the
  # other words, but that fails for "twas", and probably other things.
  return(
    !(original_word %in% breakdown)
  )
}


# .check_inflection_ending -------------------------------------------------

#' Check that Inflection is Where it is Expected
#'
#' In most regular English cases, the word should end with the inflection. But
#' there are a few exceptions of the "passersby" class, which we want to allow.
#'
#' @param original_word Character; the word before inflectional breakdown.
#' @param base_word Character; uninflected word.
#' @param inflection Character; the (putative) inflectional ending.
#'
#' @return TRUE if the word contains the candidate inflection as expected.
#' @keywords internal
.check_inflection_ending <- function(original_word, base_word, inflection) {
  # the inflectional ending generally has a hyphen at the beginning
  clean_inflection <- stringr::str_replace_all(
    inflection,
    pattern = "\\-",
    replacement = ""
  )
  if (stringr::str_ends(original_word, clean_inflection)) {
    # be explicitly lazy, since the vast majority of cases will pass this check
    return(TRUE)
  }

  if (clean_inflection != "s") {
    # the only exception we're considering now is weird plurals. I don't
    # have a current test case for this yet.
    return(FALSE) # nocov
  }

  # Insert the "s" at every position (except the end) to see if we can recover
  # the original word that way.
  inserted_match <- purrr::map_lgl(1:nchar(base_word), function(i) {
    inserted <- stringi::stri_sub_replace(
      base_word,
      from = i,
      length = 0,
      value = "s"
    )
    inserted == original_word
  })
  return(any(inserted_match))
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
