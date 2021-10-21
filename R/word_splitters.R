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

# process_word ------------------------------------------------------

#' Split a Word into Pieces
#'
#' Recursively splits a word into constituent pieces, based on Wiktionary
#' annotations. There are two main categories of word pieces used: inflections
#' (standard verb/noun/comparative adjective forms, defined in practice here as
#' endings identified in Wiktionary by inflectional function without reference
#' to the actual form of the ending) and morphemes (typically denoted in
#' Wiktionary by etymology templates).
#'
#' @inheritParams .process_word_recursive
#'
#' @return Character; the word split into pieces.
#' @export
process_word <- function(word,
                         sight_words = default_sight_words(),
                         use_lookup = TRUE) {
  return(
    # We have this unexported function purely to hide the depth stuff from the
    # end user.
    .process_word_recursive(
      word = word,
      sight_words = sight_words,
      use_lookup = use_lookup
    )
  )
}

# .process_word_recursive ------------------------------------------------------

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
#' @param sight_words Character vector; words to *not* break down further.
#'   Defaults to the included `sight_words` list; to include no sight words,
#'   pass in an empty character vector to this parameter.
#' @param current_depth Integer; current recursion depth.
#' @param max_depth Integer; maximum recursion depth.
#' @param use_lookup Logical; whether to use a cached lookup table (if
#'   available) or always process the word from scratch. If the word is not
#'   available in the lookup, processing (and likely a call to the Wiktionary
#'   API) will still occur. You might want to set this value to FALSE if you've
#'   made recent edits to Wiktionary or otherwise want to see if something has
#'   changed recently.
#' @param stop_at Character; a word to stop processing at. Used to help prevent
#'   loops.
#'
#' @return Character; the word split into pieces.
#' @keywords internal
.process_word_recursive <- function(word,
                                    sight_words = default_sight_words(),
                                    use_lookup = TRUE,
                                    current_depth = 1L,
                                    max_depth = 30L,
                                    stop_at = NULL) {
  # We expect word to be scalar, and things get weird if it isn't. Fix that and
  # warn if it isn't.
  if (length(word) > 1) {
    warning(
      "More than one word passed in for processing: ",
      paste(word, collapse = "|")
    )
    word <- word[[1]]
  }

  # If we return the original word, we want it to be a main piece, ie
  # .baseword_name, unless it already has a different name.
  names(word) <- names(word) %||% .baseword_name

  # If stop_at is set and we're processing that, send it back.
  if (!is.null(stop_at) && word == stop_at) {
    return(word)
  }

  # we never want to split short words (say, three chars or less).
  if (nchar(word) < 4L) {
    return(word)
  }
  # If word is in sight word list, stop now
  if (word %in% sight_words) {
    return(word)
  }

  if (current_depth > max_depth) {
    message(
      "maximum recursion depth of ",
      max_depth,
      " reached for word ",
      unname(word)
    )
    return(.update_env_lookup(word, word, use_lookup))
  }

  # See if we can use the lookup table.
  if (use_lookup) {
    morphemes <- .pull_from_lookup(word)
    if (length(morphemes)) {
      return(morphemes)
    }
  }

  # Get the English content once, so we don't have to hit the API multiple
  # times.
  english_content <- .fetch_english_word(word)

  # If there's no wikitext, it's not a known English word. But consider breaking
  # it!
  if (!length(english_content)) {
    breakdown <- .split_possessive(word)
    if (!length(breakdown)) {
      breakdown <- .split_on_breaks(word)
    }
    if (!length(breakdown)) {
      # There's nothing to do with this word.
      return(.update_env_lookup(word, word, use_lookup))
    }
  } else {
    # 40 words have 2 wikitext entries. For now just deal with those, we'll fix
    # that in the processor.
    english_content <- english_content[[1]]

    # Check things one at a time, if the one before didn't find anything.
    breakdown <- .split_contractions(english_content, word)
    if (!length(breakdown)) {
      breakdown <- .split_inflections(english_content, word)
    }
    if (!length(breakdown)) {
      breakdown <- .split_morphemes(english_content, word)
    }
    if (!length(breakdown)) {
      breakdown <- .check_alt_spelling(
        wt = english_content,
        word = word,
        sight_words = sight_words,
        use_lookup = use_lookup,
        current_depth = current_depth,
        max_depth = max_depth,
        stop_at = stop_at
      )
    }
    if (!length(breakdown)) {
      breakdown <- .check_misspelling(
        wt = english_content,
        word = word,
        sight_words = sight_words,
        use_lookup = use_lookup,
        current_depth = current_depth,
        max_depth = max_depth,
        stop_at = stop_at
      )
    }
    if (!length(breakdown)) {
      breakdown <- .split_possessive(
        word = word
      )
    }
    # If we don't find anything formal, split on split characters.
    if (!length(breakdown)) {
      breakdown <- .split_on_breaks(word)
    }
  }

  if (!is.null(stop_at) && stop_at %in% breakdown) {
    return(stop_at)
  }

  if (length(breakdown)) {
    breakdown <- .clean_output(breakdown)

    all_pieces <- purrr::map(
      breakdown,
      .process_word_recursive,
      sight_words = sight_words,
      use_lookup = use_lookup,
      current_depth = current_depth + 1L,
      max_depth = max_depth,
      stop_at = stop_at
    )

    processed_word <- .correct_names(all_pieces)
    processed_word <- .fix_hyphens(processed_word)

    return(.update_env_lookup(word, processed_word, use_lookup))
  } else {
    return(.update_env_lookup(word, word, use_lookup))
  }
}

# .split_inflections ------------------------------------------------------

#' Split Standard Verb, Noun, and Adjective Endings of a Word
#'
#' @inheritParams .process_word_recursive
#' @param english_content The return from \code{\link{.fetch_english_word}}.
#'
#' @return Character; the word with standard endings split off.
#' @keywords internal
.split_inflections <- function(english_content, word) {
  if (length(english_content) == 0) {
    return(character(0))
  }

  if (.detect_irregular_wt(english_content)) {
    # for now, return irregular words without attempting to split inflections.
    return(character(0)) # nocov
  }

  # Wiktionary has a variety of templates for various standard endings. In some
  # cases there are multiple ways to denote word structure. Maybe later make
  # utility function to maintain these?
  # https://github.com/macmillancontentscience/wikimorphemes/issues/11
  patterns_endings <- c(
    # <pattern_to_detect> = <standard_ending>
    "\\{\\{plural of\\|en\\|([^}]+)\\}\\}" = "-s",
    "\\{\\{en-third-person singular of\\|([^\\|\\}]+)" = "-s",
    "\\{\\{en-ing form of\\|([^\\|\\}]+)" = "-ing", # "escaping"
    "\\{\\{present participle of\\|en\\|([^\\|\\}]+)" = "-ing",
    # I don't think this is a very general pattern. Maybe replace with
    # {{inflection of| }} template?
    "The action of the verb '''to \\[\\[([^\\]]+)\\]\\]'''" = "-ing",
    "\\{\\{past participle of\\|en\\|([^\\|\\}]+)" = "-ed",
    "\\{\\{en-past of\\|([^\\|\\}]+)" = "-ed",
    "\\{\\{en-comparative of\\|([^\\|\\}]+)" = "-er",
    "\\{\\{en-superlative of\\|([^\\|\\}]+)" = "-est"
  )

  candidate_breakdowns <- vector(mode = "list")
  for (patt in names(patterns_endings)) {
    ending <- patterns_endings[[patt]]
    base_word <- stringr::str_match(english_content, patt)[[2]]
    if (!is.na(base_word)) {
      base_word <- .clean_word_reference(base_word)
      if (.check_reconstructed_word(word, base_word, ending) &
        .check_nonexplosive_word(word, base_word, ending) &
        # word should actually end with ending. No "ridden" -> "ride -ed"!
        .check_inflection_ending(word, base_word, ending)) {
        breakdown <- c(base_word, ending)
        names(breakdown) <- c(.baseword_name, .inflection_name)
        candidate_breakdowns[[length(candidate_breakdowns) + 1]] <- breakdown
      }
    }
  }
  if (length(candidate_breakdowns) == 0) {
    return(character(0))
  }
  unique_breakdowns <- unique(candidate_breakdowns)
  # if (length(unique_breakdowns) > 1) {
  #   # https://github.com/macmillancontentscience/wikimorphemes/issues/14
  #   warning("more than one unique breakdown found for: ", word) # nocov
  # }
  return(unique_breakdowns[[1]])
}

#' Strip off Deep Links
#'
#' Words can link to specific sections. Any time we return a word, we really
#' just care about the word, not the deeper link.
#'
#' @param word The word (extracted from a template, probably) to clean.
#'
#' @return Just the word.
#' @keywords internal
.clean_word_reference <- function(word) {
  # I want this to be general-purpose, so let's not assume "word" is a single
  # word. But we ARE assuming the names don't matter, if it has any, because
  # every potential use of this is before we name things.
  return(
    purrr::map_chr(
      word,
      ~ stringr::str_split(.x, "#", simplify = TRUE)[[1]]
    )
  )
}


# .split_morphemes ------------------------------------------------------

#' Split Word into Morphemes
#'
#' @inheritParams .split_inflections
#'
#' @return Character; the word split into morphemes.
#' @keywords internal
.split_morphemes <- function(english_content, word) {
  if (length(english_content) == 0) {
    return(character(0)) # nocov
  }

  candidate_breakdowns <- list(
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
    return(character(0))
  }
  unique_breakdowns <- unique(candidate_breakdowns)
  # if (length(unique_breakdowns) > 1) {
  #   # https://github.com/macmillancontentscience/wikimorphemes/issues/14
  #   warning("more than one unique breakdown found for: ", word) # nocov
  # }
  return(.clean_output(unique_breakdowns[[1]]))
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
  breakdown <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(
    string = breakdown,
    pattern = "=", negate = TRUE
  )
  breakdown <- .clean_word_reference(breakdown)
  if (!is.na(match)) {
    # `breakdown` should be length-2, but template might be badly formatted.
    if (length(breakdown) != 2) {
      #  https://github.com/macmillancontentscience/wikimorphemes/issues/10
      return(character(0)) # nocov
    }
    # At this point in the process, apply standard that prefixes end in "-"
    breakdown[[1]] <- paste0(breakdown[[1]], "-")
    names(breakdown) <- c(.prefix_name, .baseword_name)
    # standardize naming:
    # https://github.com/macmillancontentscience/wikimorphemes/issues/7
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
  breakdown <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(
    string = breakdown,
    pattern = "=", negate = TRUE
  )
  breakdown <- .clean_word_reference(breakdown)
  if (!is.na(match)) {
    # `breakdown` should be length-2, but template might be badly formatted.
    if (length(breakdown) != 2) {
      #  https://github.com/macmillancontentscience/wikimorphemes/issues/10
      return(character(0)) # nocov
    }
    # At this point in the process, apply standard that suffixes begin with "-"
    breakdown[[2]] <- paste0("-", breakdown[[2]])
    # It is *possible* that a suffix has further suffix breakdowns, e.g.
    # "-ization" -> "-ize" + "-ation"
    # If the original word starts with a hyphen, tag it as a suffix rather than
    # as a base word.
    names(breakdown) <- c(.baseword_name, .suffix_name)
    if (stringr::str_starts(breakdown[[1]], "\\-")) {
      names(breakdown) <- c(.suffix_name, .suffix_name)
    }
    # standardize naming:
    # https://github.com/macmillancontentscience/wikimorphemes/issues/7
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
  breakdown <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(
    string = breakdown,
    pattern = "=", negate = TRUE
  )
  breakdown <- .clean_word_reference(breakdown)
  # https://github.com/macmillancontentscience/wikimorphemes/issues/10
  if (!is.na(match)) {
    # This one is tricky. If a piece ends with "-", assign name "prefix". If
    # starts with "-", assign name "suffix". If "-" on both sides, "interfix".
    # If no "-", it's a "base_word".
    name_list <- rep(.baseword_name, length(breakdown))
    name_list[stringr::str_starts(breakdown, "-")] <- .suffix_name
    name_list[stringr::str_ends(breakdown, "-")] <- .prefix_name
    name_list[stringr::str_ends(breakdown, "-") &
      stringr::str_starts(breakdown, "-")] <- .interfix_name
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
  breakdown <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(
    string = breakdown,
    pattern = "=", negate = TRUE
  )
  breakdown <- .clean_word_reference(breakdown)
  # https://github.com/macmillancontentscience/wikimorphemes/issues/10
  if (!is.na(match)) {
    # if only two pieces, should be prefix + suffix
    if (length(breakdown) == 2) {
      # At this point in the process, apply standard that suffixes begin with
      # "-", and prefixes end with "-"
      breakdown[[1]] <- paste0(breakdown[[1]], "-")
      breakdown[[2]] <- paste0("-", breakdown[[2]])
      names(breakdown) <- c(.prefix_name, .suffix_name)
      # standardize names:
      # https://github.com/macmillancontentscience/wikimorphemes/issues/7
    } else if (length(breakdown) == 3) { # nocov start
      # I can't find an example that tests this. "bedewed" uses this template,
      # but gets caught by inflection splitter first.
      breakdown[[1]] <- paste0(breakdown[[1]], "-")
      breakdown[[3]] <- paste0("-", breakdown[[3]])
      names(breakdown) <- c(.prefix_name, .baseword_name, .suffix_name)
    } else {
      return(character(0))
      # https://github.com/macmillancontentscience/wikimorphemes/issues/10
    } # nocov end
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
  breakdown <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # take out named parameters (marked with "=")
  breakdown <- stringr::str_subset(
    string = breakdown,
    pattern = "=", negate = TRUE
  )
  breakdown <- .clean_word_reference(breakdown)
  # https://github.com/macmillancontentscience/wikimorphemes/issues/10
  if (!is.na(match)) {
    # all components should be tagged as base words
    # !!  Find a better way that doesn't involve repeated names?
    names(breakdown) <- rep(.baseword_name, length(breakdown))
  }

  return(breakdown)
}


# .split_contractions_wt ------------------------------------------------------

#' Split Contractions
#'
#' @param wt Character; wikitext of a word
#' @param word Character; the word.
#'
#' @return Character; the word split up into component words.
#' @keywords internal
.split_contractions <- function(wt, word) {
  # word <- "'twas"
  # word <- "'twasn't"
  # word <- "they're"
  # word <- "would've"
  # word <- "wouldn't"
  # word <- "wouldn't've"
  # word <- "'dillo"
  # word <- "wouldn't"
  # word <- "wouldn't've"
  # wt <- .fetch_english_word(word)

  # Only process contractions if the word still contains an apostrophe,
  # otherwise it might be a historical contraction that's weird now.
  if (stringr::str_detect(word, "'")) {
    # There are two contraction patterns: 1 in the etymology, 1 in the
    # definition. If a word has both, we want the etymology one.
    patt <- .make_template_pattern("contraction")
    match <- stringr::str_match_all(wt, patt)[[1]]

    # If that one didn't work, try the other one.
    if (nrow(match) == 0) {
      patt <- .make_template_pattern("contraction of")
      match <- stringr::str_match_all(wt, patt)[[1]]
    }

    # If there's exactly one contraction for this word, clean it up and return
    # it.
    if (nrow(match) == 1) {
      match <- match[[2]]

      # There are a couple ways to use these templates, so make sure we deal
      # with all of them.
      breakdown <- stringr::str_split(
        string = match,
        pattern = stringr::coll("|")
      )[[1]]
      # Take out named parameters (marked with "=").
      breakdown <- stringr::str_subset(
        string = breakdown,
        pattern = "=", negate = TRUE
      )

      # Sometimes the words are wrapped in [[]], sometimes they aren't. As far
      # as I've seen it's either [[]] *or* | so this should catch things
      # properly.
      if (stringr::str_detect(breakdown[[1]], stringr::fixed("["))) {
        breakdown <- stringr::str_match_all(
          breakdown[[1]],
          pattern = "\\[\\[([^]]+)\\]\\]"
        )[[1]][, 2]
      }

      breakdown <- .clean_word_reference(breakdown)

      breakdown <- breakdown[
        purrr::map_lgl(breakdown, function(bd) {
          .check_nonexplosive_word(word, bd)
        })
      ]

      if (length(breakdown)) {
        # all components should be tagged as base words
        names(breakdown) <- rep(.baseword_name, length(breakdown))
        return(breakdown)
      }
    }
  }

  return(character(0))
}

# .check_alt_spelling ------------------------------------------------------

#' Check for Alternative Spelling
#'
#' Checks the wikitext for an alternative spelling.
#'
#' @inheritParams .process_word_recursive
#' @param wt Character; wikitext of a word
#' @param word Character; the word, to make sure this isn't a loop.
#'
#' @return Character; the first-level of the alternate word or words.
#' @keywords internal
.check_alt_spelling <- function(wt,
                                word,
                                sight_words,
                                use_lookup,
                                current_depth,
                                max_depth,
                                stop_at) {
  # word <- "passerby"
  # word <- "clearinghouse"
  # word <- "metaanalysis"
  # word <- "auroch"
  # wt <- .fetch_english_word(word)

  # Maybe also catch "alternative form of" here?
  asp_patt <- .make_template_pattern("alternative spelling of")
  match <- stringr::str_match(wt, asp_patt)[[2]]
  # First split out the template parameters.
  alt_word <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # Take out named parameters (marked with "=").
  alt_word <- stringr::str_subset(
    string = alt_word,
    pattern = "=", negate = TRUE
  )
  alt_word <- .clean_word_reference(alt_word)

  if (length(alt_word)) {
    stop_at <- stop_at %||% word
    # Conceivably it could be an alternative spelling of more than one thing.
    # Let's just use the first one. It can also have parts that are descriptions
    # of the relationship, such as this from pause: "Pause||a button that pauses
    # or resumes something"
    alt_word <- alt_word[[1]]

    breakdown <- .process_word_recursive(
      word = alt_word,
      sight_words = sight_words,
      use_lookup = use_lookup,
      current_depth = current_depth + 1L,
      max_depth = max_depth,
      stop_at = stop_at
    )

    # Just send the alternative form through to be re-processed.
    if (
      length(breakdown) && .check_nonexplosive_word(word, breakdown)) {
      return(breakdown)
    }
  }

  #  https://github.com/macmillancontentscience/wikimorphemes/issues/10
  return(character(0))
}


# .check_misspelling_wt ------------------------------------------------------

#' Check for Misspelling
#'
#' Checks the wikitext to see if this is a misspelling of another word.
#'
#' @inheritParams .process_word_recursive
#' @param wt Character; wikitext of a word
#' @param word Character; the word, to make sure we don't loop.
#'
#' @return Character; the better spelling of the word.
#' @keywords internal
.check_misspelling <- function(wt,
                               word,
                               sight_words,
                               use_lookup,
                               current_depth,
                               max_depth,
                               stop_at) {
  # word <- "twas"
  # word <- "abhoring"
  # word <- "prejudice"
  # wt <- .fetch_english_word(word)
  patt <- .make_template_pattern("misspelling of")
  match <- stringr::str_match(wt, patt)[[2]]
  # First split out the template parameters.
  alt_word <- stringr::str_split(
    string = match,
    pattern = stringr::coll("|")
  )[[1]]
  # Take out named parameters (marked with "=").
  alt_word <- stringr::str_subset(
    string = alt_word,
    pattern = "=", negate = TRUE
  )
  alt_word <- .clean_word_reference(alt_word)

  if (length(alt_word)) {
    stop_at <- stop_at %||% word
    # Conceivably it could be a misspelling of more than one thing.
    # Let's just use the first one.
    alt_word <- alt_word[[1]]

    breakdown <- .process_word_recursive(
      word = alt_word,
      sight_words = sight_words,
      use_lookup = use_lookup,
      current_depth = current_depth + 1L,
      max_depth = max_depth,
      stop_at = stop_at
    )

    # Send the breakdown through to be re-processed.
    if (length(breakdown) && .check_nonexplosive_word(word, breakdown)) {
      return(breakdown)
    }
  }

  #  https://github.com/macmillancontentscience/wikimorphemes/issues/10
  return(character(0))
}



#' Split the Possessive s
#'
#' This is probably not exhaustive, and could potentially have some false
#' positives, but it should only happen when templates fail us so I think it
#' will usually be correct.
#'
#' @param word The word to split.
#'
#' @return The word, split into word + -'s if it is possessive.
#' @keywords internal
.split_possessive <- function(word) {
  if (
    stringr::str_detect(word, "^[^']+'s$") ||
    stringr::str_detect(word, "^[^']+[sx]'$")
  ) {
    breakdown <- stringr::str_split(
      word,
      "'"
    )[[1]][[1]]
    breakdown <- c(
      breakdown,
      "-'s"
    )
    # I'm calling this a suffix but it's really a particle, I think?
    names(breakdown) <- c(names(word), "suffix")
    return(breakdown)
  }
  return(character(0))
}

#' Split on Break Characters
#'
#' This is a fall-through if nothing else breaks the word down.
#'
#' @param word The word to potentially break down.
#'
#' @return The breakdown, if there are break characters.
#' @keywords internal
.split_on_breaks <- function(word) {
  # This is more complicated than it appears, because we want to keep "-" at the
  # beginning or end of the word.
  if (stringr::str_detect(word, "[^ -][ -][^ -]")) {
    breakdown <- stringr::str_split(
      word,
      "[ -]"
    )
    breakdown <- .clean_output(breakdown)
    if (stringr::str_starts(word, "-")) {
      breakdown[[1]] <- paste0("-", breakdown[[1]])
    }
    if (stringr::str_ends(word, "-")) {
      breakdown[[length(breakdown)]] <- paste0(
        breakdown[[length(breakdown)]],
        "-"
      )
    }
    names(breakdown) <- rep(names(word), length(breakdown))
    return(breakdown)
  }
  return(character(0))
}

# .clean_output ------------------------------------------------------

#' Clean word-splitter output
#'
#' Sometimes the output from the word-splitting routines has unexpected
#' characters in it, such as spaces or apostrophes. To avoid downstream issues,
#' we should do some clean-up on the output before continuing.
#'
#' @param split_word Character vector; the output from `.split_morphemes` or
#'   similar.
#'
#' @return Character vector; the output split on any unexpected characters.
#' @keywords internal
.clean_output <- function(split_word) {
  # These are relatively rare cases. The `'` example has already been
  # fixed in Wiktionary, but we should catch it just in case.
  # This also takes care of the problem of infinitely recursing on, e.g.,
  # "cyber-" due to empty string after the hyphen.

  # I'm pretty sure we just want to split on space now.
  split_more <- unlist(
    purrr::map(
      split_word,
      stringr::str_split,
      pattern = "\\s+"
    ),
    use.names = TRUE
  )

  # we *don't* want to remove hyphens at this point, as they are used to
  # indicate affixes.
  # split_more <- unlist(
  #   purrr::map(
  #     split_word,
  #     stringr::str_split,
  #     pattern = "[^[:alpha:]\\-]"
  #   ),
  #   use.names = TRUE
  # )
  # # Get rid of weird things generated during the additional split.
  split_more <- split_more[!(split_more %in% c("", "-"))]

  # we're a little naughty and use non-unique names.
  names(split_more) <- stringr::str_remove_all(names(split_more), "[0-9]")
  return(split_more)
}

#' Deal with Piece Names
#'
#' @param all_pieces A morpheme breakdown, which will likely be a 2-level named
#'   list.
#'
#' @return The morpheme breakdown as a character vector with the desired names.
#' @keywords internal
.correct_names <- function(all_pieces) {
  processed_names <- purrr::imap(
    all_pieces,
    function(this_piece, this_name) {
      if (length(this_piece) == 1) {
        names(this_piece) <- this_name
      }
      return(this_piece)
    }
  )
  return(purrr::flatten_chr(processed_names))
}
