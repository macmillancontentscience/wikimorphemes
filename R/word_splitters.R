# process_word ------------------------------------------------------

#' Split a Word into Pieces
#'
#' Splits a word into constituent pieces. Wrapper around
#' \code{\link{process_word_recursive}}, with some postprocessing.
#'
#' @inheritParams process_word_recursive
#'
#' @return Character; the word split into pieces.
#' @export
process_word <- function(word) {
  processed_word_0 <- process_word_recursive(word)
  # This is the place where we want to remove hyphens. But then we need to add
  # the names back on.
  processed_word <- stringr::str_remove_all(processed_word_0, "\\-")
  names(processed_word) <- names(processed_word_0)

  # Fill in missing name for single words.
  if (length(processed_word) == 1) {
    names(processed_word) <- .baseword_name
  }
  return(processed_word)
}

# process_word_recursive ------------------------------------------------------

#' Split a Word into Pieces
#'
#' Recursively splits a word into constituent pieces, based on Wiktionary
#' annotations. There are two main categories of word pieces used: inflections
#' (standard verb/noun/comparative adjective forms, defined in practice here as
#' endings identified in Wiktionary by inflectional function without reference
#' to the actual form of the ending) and morphemes (typically denoted in
#' Wiktionary by etymology templates).
#'
#' @param word Character; a word to process.
#' @param current_depth Integer; current recursion depth.
#' @param max_depth Integer; maximum recursion depth.
#'
#' @return Character; the word split into pieces.
#' @export
process_word_recursive <- function(word, current_depth = 1, max_depth = 30) {
  if (current_depth > max_depth) {
    message("maximum recursion depth of ", max_depth, " reached.")
    return(word)
  }

  # First check for inflections.
  inf_break <- split_inflections(word)
  if (length(inf_break) == 2) {
    # keep processing base_word (inflection endings need no further processing)
    return(c(process_word_recursive(inf_break[1],
                                    current_depth = current_depth + 1,
                                    max_depth = max_depth),
             inf_break[2]))
  }
  # If we made it here, no inflections found. Check for morphemes...
  mor_break <- split_morphemes(word)
  if (length(mor_break) >= 2) {
    # process all pieces, including prefixes, etc.
    all_pieces <- purrr::map(
      mor_break,
      process_word_recursive,
      current_depth = current_depth + 1,
      max_depth = max_depth
    )
    processed_word <- unlist(all_pieces)
    # Fix names by dropping everything before the last "." and removing digits.
    names(processed_word) <- stringr::str_remove_all(names(processed_word),
                                                     "(.*\\.)|[0-9]*")
    return(processed_word)
  }
  # If we made it here, neither inflections nor morphemes found.
  return(word)
}

# split_inflections ------------------------------------------------------

#' Split Standard Verb, Noun, and Adjective Endings of a Word
#'
#' @param word Character; a word to process.
#'
#' @return Character; the word with standard endings split off.
#' @export
split_inflections <- function(word) {
  english_content <- .fetch_english_word(word)
  if (length(english_content) == 0) {
    # not an english word
    # return word here ? https://github.com/jonthegeek/wikimorphemes/issues/10
    return(character(0))
  }

  if (.detect_irregular_wt(english_content)) {
    # for now, return irregular words without attempting to split inflections.
    return(word)
  }

  # Wiktionary has a variety of templates for various standard endings. In some
  # cases there are multiple ways to denote word structure. Maybe later make
  # utility function to maintain these?
  # https://github.com/jonthegeek/wikimorphemes/issues/11
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
      if (.check_reconstructed_word(word, base_word, ending) &
          .check_nonexplosive_word(word, base_word, ending)) {
        breakdown <- c(base_word, ending)
        names(breakdown) <- c(.baseword_name, .inflection_name)
        candidate_breakdowns[[length(candidate_breakdowns)+1]] <- breakdown
      }
    }
  }
  if (length(candidate_breakdowns) == 0) {
    return(word)
  }
  unique_breakdowns <- unique(candidate_breakdowns)
  if (length(unique_breakdowns) > 1) {
    # https://github.com/jonthegeek/wikimorphemes/issues/14
    warning("more than one unique breakdown found for: ", word) # nocov
  }
  return(unique_breakdowns[[1]])
}


# split_morphemes ------------------------------------------------------

#' Split Word into Morphemes
#'
#' @param word Character; a word to process.
#'
#' @return Character; the word split into morphemes.
#' @export
split_morphemes <- function(word) {
  # I might want to make these functions act on the wikitext, not the actual
  # word, so that we can limit calls to wiktionary API. Or maybe somehow cache
  # results in session?
  # https://github.com/jonthegeek/wikimorphemes/issues/13
  english_content <- .fetch_english_word(word)
  if (length(english_content) == 0) {
    # not an english word
    #  https://github.com/jonthegeek/wikimorphemes/issues/10
    return(character(0))
  }

  candidate_breakdowns <- list(
    .check_alt_spelling_wt(english_content),
    .split_affixes_wt(english_content),
    .split_prefixes_wt(english_content),
    .split_suffixes_wt(english_content),
    .split_compounds_wt(english_content),
    .split_confixes_wt(english_content)
  )

  # Take out empty cases. This feels too messy.
  candidate_breakdowns <- candidate_breakdowns[
    purrr::map_lgl(candidate_breakdowns, rlang::has_length)
    ]
  # Take out candidates that are too far from original, or explosive.
  candidate_breakdowns <- candidate_breakdowns[
    purrr::map_lgl(candidate_breakdowns, function(bd) {
      .check_reconstructed_word(word, bd) &
        .check_nonexplosive_word(word, bd)
    })
    ]

  if (length(candidate_breakdowns) == 0) {
    return(word)
  }
  unique_breakdowns <- unique(candidate_breakdowns)
  if (length(unique_breakdowns) > 1) {
    # https://github.com/jonthegeek/wikimorphemes/issues/14
    warning("more than one unique breakdown found for: ", word) # nocov
  }
  return(unique_breakdowns[[1]])
}


# .split_prefixes_wt ------------------------------------------------------

#' Split Prefixes from a Word
#'
#' @param wt Character; wikitext of a word
#'
#' @return Character; the word with prefixes split off.
#' @keywords internal
.split_prefixes_wt <- function(wt) {
  # wt <- .fetch_english_word("object")
  prefix_patt <- .make_template_pattern("pre(fix)?")
  # Take 3rd element to account for optional capturing group: (fix)?
  match <- stringr::str_match(wt, prefix_patt)[[3]]
  breakdown <- stringr::str_split(string = match,
                                  pattern = stringr::coll("|"))[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(string = breakdown,
                                   pattern = "=", negate = TRUE)
  if (!is.na(match)) {
    # `breakdown` should be length-2, but template might be badly formatted.
    if (length(breakdown) != 2) {
      #  https://github.com/jonthegeek/wikimorphemes/issues/10
      return(character(0))
    }
    # At this point in the process, apply standard that prefixes end in "-"
    breakdown[[1]] <- paste0(breakdown[[1]], "-")
    names(breakdown) <- c(.prefix_name, .baseword_name)
    # standardize naming: https://github.com/jonthegeek/wikimorphemes/issues/7
  }

  return(breakdown)
}



# .split_suffixes_wt ------------------------------------------------------

#' Split Suffixes from a Word
#'
#' @param wt Character; wikitext of a word
#'
#' @return Character; the word with suffixes split off.
#' @keywords internal
.split_suffixes_wt <- function(wt) {
  # wt <- .fetch_english_word("happily")
  suffix_patt <- .make_template_pattern("suf(fix)?")
  # Take 3rd element to account for optional capturing group: (fix)?
  match <- stringr::str_match(wt, suffix_patt)[[3]]
  breakdown <- stringr::str_split(string = match,
                                  pattern = stringr::coll("|"))[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(string = breakdown,
                                   pattern = "=", negate = TRUE)
  if (!is.na(match)) {
    # `breakdown` should be length-2, but template might be badly formatted.
    if (length(breakdown) != 2) {
      #  https://github.com/jonthegeek/wikimorphemes/issues/10
      return(character(0))
    }
    # At this point in the process, apply standard that suffixes begin with "-"
    breakdown[[2]] <- paste0("-", breakdown[[2]])
    names(breakdown) <- c(.baseword_name, .suffix_name)
    # standardize naming: https://github.com/jonthegeek/wikimorphemes/issues/7
  }

  return(breakdown)
}



# .split_affixes_wt ------------------------------------------------------

#' Split Affixes from a Word
#'
#' @param wt Character; wikitext of a word
#'
#' @return Character; the word with affixes split off.
#' @keywords internal
.split_affixes_wt <- function(wt) {
  # wt <- .fetch_english_word("pesticide")
  # wt <- .fetch_english_word("volleyball")
  affix_patt <- .make_template_pattern("af(fix)?")
  # Take 3rd element to account for optional capturing group: (fix)?
  match <- stringr::str_match(wt, affix_patt)[[3]]
  breakdown <- stringr::str_split(string = match,
                                  pattern = stringr::coll("|"))[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(string = breakdown,
                                   pattern = "=", negate = TRUE)
  if (!is.na(match)) { #  https://github.com/jonthegeek/wikimorphemes/issues/10
    # This one is tricky. If a piece ends with "-", assign name "prefix". If
    # starts with "-", assign name "suffix". If "-" on both sides, "interfix".
    # If no "-", it's a "base_word".
    name_list <- rep(.baseword_name, length(breakdown))
    name_list[stringr::str_starts(breakdown, "-")] <- .suffix_name
    name_list[stringr::str_ends(breakdown, "-")] <- .prefix_name
    name_list[stringr::str_ends(breakdown, "-") &
                stringr::str_starts(breakdown, "-")] <- .interfix_name
    # # now remove hyphens from breakdown. No, not now. We keep hyphens in
    # word pieces at this point so that in the recursive breakdown
    # prefixes, etc get processed appropriately (e.g. so "-mas" doesn't
    # turn into "ma s").
    # breakdown <- stringr::str_remove_all(breakdown, "\\-")
    names(breakdown) <- name_list
  }

  return(breakdown)
}



# .split_confixes_wt ------------------------------------------------------

#' Split Confixes from a Word
#'
#' In Wiktionary, words with both a suffix and prefix are tagged as "confix".
#' Also used when there is no base word (only prefix + suffix)
#'
#' @param wt Character; wikitext of a word
#'
#' @return Character; the word with confixes split off.
#' @keywords internal
.split_confixes_wt <- function(wt) {
  # wt <- .fetch_english_word("neurogenic")
  # wt <- .fetch_english_word("bedewed")
  confix_patt <- .make_template_pattern("con(fix)?")
  # Take 3rd element to account for optional capturing group: (fix)?
  match <- stringr::str_match(wt, confix_patt)[[3]]
  breakdown <- stringr::str_split(string = match,
                                  pattern = stringr::coll("|"))[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(string = breakdown,
                                   pattern = "=", negate = TRUE)
  if (!is.na(match)) { #  https://github.com/jonthegeek/wikimorphemes/issues/10
    # if only two pieces, should be prefix + suffix
    if (length(breakdown) == 2) {
      # At this point in the process, apply standard that suffixes begin with
      # "-", and prefixes end with "-"
      breakdown[[1]] <- paste0(breakdown[[1]], "-")
      breakdown[[2]] <- paste0("-", breakdown[[2]])
      names(breakdown) <- c(.prefix_name, .suffix_name)
      # standardize names: https://github.com/jonthegeek/wikimorphemes/issues/7

    } else if (length(breakdown) == 3) { # nocov start
      # I can't find an example that tests this. "bedewed" uses this template,
      # but gets caught by inflection splitter first.
      breakdown[[1]] <- paste0(breakdown[[1]], "-")
      breakdown[[3]] <- paste0("-", breakdown[[3]])
      names(breakdown) <- c(.prefix_name, .baseword_name, .suffix_name)
    } else {
      return(character(0))
      #https://github.com/jonthegeek/wikimorphemes/issues/10
    }  # nocov end
  }

  return(breakdown)
}




# .split_compounds_wt ------------------------------------------------------

#' Split Compound Word
#'
#' @param wt Character; wikitext of a word
#'
#' @return Character; the word split up into component words.
#' @keywords internal
.split_compounds_wt <- function(wt) {
  # wt <- .fetch_english_word("rainbow")
  # wt <- .fetch_english_word("summertime")
  comp_patt <- .make_template_pattern("com(pound)?")
  # Take 3rd element to account for optional capturing group: (pound)?
  match <- stringr::str_match(wt, comp_patt)[[3]]
  breakdown <- stringr::str_split(string = match,
                                  pattern = stringr::coll("|"))[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(string = breakdown,
                                   pattern = "=", negate = TRUE)
  if (!is.na(match)) { #  https://github.com/jonthegeek/wikimorphemes/issues/10
    # all components should be tagged as base words
    # !!  Find a better way that doesn't involve repeated names?
    names(breakdown) <- rep(.baseword_name, length(breakdown))
  }

  return(breakdown)
}




# .check_alt_spelling_wt ------------------------------------------------------

#' Check for Simpler Alternative Spelling
#'
#' Checks the wikitext for an alternative spelling that breaks the word up into
#' smaller pieces (e.g. "passerby" -> "passer-by").
#'
#' @param wt Character; wikitext of a word
#'
#' @return Character; the word split up into component words.
#' @keywords internal
.check_alt_spelling_wt <- function(wt) {
  # wt <- .fetch_english_word("passerby")
  # wt <- .fetch_english_word("clearinghouse")
  asp_patt <- .make_template_pattern("alternative spelling of")
  match <- stringr::str_match(wt, asp_patt)[[2]]
  # First split out the template parameters.
  breakdown <- stringr::str_split(string = match,
                                  pattern = stringr::coll("|"))[[1]]
  # Take out named parameters (marked with "=").
  breakdown <- stringr::str_subset(string = breakdown,
                                   pattern = "=", negate = TRUE)
  # Alternative spelling is "simpler" if there's a space or hyphen.
  #TODO Decide whether there are other cases we want to take alt. spelling.
  if (isTRUE(stringr::str_detect(string = breakdown, pattern = "\\s|\\-"))) {
    breakdown <- stringr::str_split(string = breakdown,
                                    pattern = "\\s|\\-")[[1]]
    # all components should be tagged as base words
    names(breakdown) <- rep(.baseword_name, length(breakdown))
    return(breakdown)
  }
  #  https://github.com/jonthegeek/wikimorphemes/issues/10
  return(character(0))
}


