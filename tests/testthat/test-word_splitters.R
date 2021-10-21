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

test_that(".split_inflections works", {
  # Set up a function so we don't have to fetch each time, now that the fetch
  # happens outside of the .split_ functions. I'm starting it with .test_ to
  # avoid any possible collisions.
  .test_fetch_then_split <- function(word) {
    english_content <- .fetch_english_word(word)
    return(.split_inflections(english_content, word))
  }

  testthat::expect_identical(
    .test_fetch_then_split("lighter"),
    c(base_word = "light", inflection = "-er")
  )

  testthat::expect_identical(
    .test_fetch_then_split("lightest"),
    c(base_word = "light", inflection = "-est")
  )

  testthat::expect_identical(
    .test_fetch_then_split("lighting"),
    c(base_word = "light", inflection = "-ing")
  )

  testthat::expect_identical(
    .test_fetch_then_split("lights"),
    c(base_word = "light", inflection = "-s")
  )

  # This test fails with the wikitext dump cache until the new one is processed.
  # It is known. Hiding the cache makes it pass.
  testthat::expect_identical(
    .test_fetch_then_split("running"),
    c(base_word = "run", inflection = "-ing")
  )

  testthat::expect_identical(
    .test_fetch_then_split("scrapped"),
    c(base_word = "scrap", inflection = "-ed")
  )

  testthat::expect_identical(
    .test_fetch_then_split("scraped"),
    c(base_word = "scrape", inflection = "-ed")
  )

  testthat::expect_identical(
    .test_fetch_then_split("escaping"),
    c(base_word = "escape", inflection = "-ing")
  )

  # non-splittable words come back as character(0)
  testthat::expect_identical(
    .test_fetch_then_split("escape"),
    character(0)
  )

  # non-English words come back as character(0), too
  testthat::expect_identical(
    .test_fetch_then_split("bueno"),
    character(0)
  )

  # irregular words come back as character(0)
  testthat::expect_identical(
    .test_fetch_then_split("ground"),
    character(0)
  )

  # unclassified irregulars are caught by stringdist check.
  testthat::expect_identical(
    .test_fetch_then_split("best"),
    character(0)
  )
})

test_that("process_word works", {
  testthat::expect_identical(
    process_word("upendings", use_lookup = FALSE),
    c(prefix = "up-", base_word = "end", inflection = "-ing", inflection = "-s")
  )

  testthat::expect_identical(
    process_word("disestablishmentarianism", use_lookup = FALSE),
    c(
      prefix = "dis-", base_word = "establish",
      suffix = "-ment", suffix = "-arian", suffix = "-ism"
    )
  )

  testthat::expect_identical(
    process_word("unaffable", use_lookup = FALSE),
    c(prefix = "un-", base_word = "affable")
  )

  testthat::expect_identical(
    process_word("pesticides", use_lookup = FALSE),
    c(base_word = "pest", interfix = "-i-", suffix = "-cide", inflection = "-s")
  )

  # composite suffixes get named appropriately
  testthat::expect_identical(
    process_word("tokenization", use_lookup = FALSE),
    c(base_word = "token", suffix = "-ize", suffix = "-ation")
  )

  testthat::expect_identical(
    process_word("neurogenic", use_lookup = FALSE),
    # Think about breakdown of "genic" into "gene ic". Genic was aready marked
    # as a suffix; should it be broken further? For now, "-genic" is *not* split
    # further. Note that this is how it is on wiktionary, not something *we*
    # decide.
    c(prefix = "neuro-", suffix = "-genic")
  )

  testthat::expect_identical(
    process_word("bedewed", use_lookup = FALSE),
    c(prefix = "be-", base_word = "dew", inflection = "-ed")
  )

  testthat::expect_identical(
    process_word("rainbow", use_lookup = FALSE),
    c(base_word = "rain", base_word = "bow")
  )

  testthat::expect_identical(
    process_word("clearinghouse", use_lookup = FALSE),
    c(base_word = "clear", inflection = "-ing", base_word = "house")
  )

  testthat::expect_identical(
    process_word("metaanalysis", use_lookup = FALSE),
    process_word("meta-analysis", use_lookup = FALSE)
  )

  testthat::expect_identical(
    process_word("auroch", use_lookup = FALSE),
    c(base_word = "auroch")
  )

  testthat::expect_identical(
    process_word("aurochs", use_lookup = FALSE),
    c(base_word = "auroch", inflection = "-s")
  )

  testthat::expect_identical(
    process_word("Labor", use_lookup = FALSE),
    c(base_word = "Labor")
  )

  # We now require inflected words to actually end with their inflection, but
  # make an exception to accommodate weird plurals like "passersby"
  testthat::expect_identical(
    process_word("passersby", use_lookup = FALSE),
    c(base_word = "pass", suffix = "-er", base_word = "by", inflection = "-s")
  )

  # DON'T process "-mas" into "ma s"
  testthat::expect_identical(
    process_word("Christmas", use_lookup = FALSE),
    c(base_word = "Christ", suffix = "-mas")
  )

  testthat::expect_identical(
    process_word("every", use_lookup = FALSE),
    c(base_word = "every")
  )

  testthat::expect_identical(
    process_word("lenses", use_lookup = FALSE),
    c(base_word = "lens", inflection = "-s")
  )

  # check recursion limit
  testthat::expect_message(
    .process_word_recursive("lovingly", max_depth = 1, use_lookup = FALSE),
    "maximum recursion depth of 1 reached"
  )

  # The sight words list changes some things...
  testthat::expect_identical(
    process_word(
      "into",
      sight_words = default_sight_words(),
      use_lookup = FALSE
    ),
    c(base_word = "into") # without sight words: "in" + "to"
  )

  # Contractions should work.
  testthat::expect_identical(
    process_word(
      "they're",
      use_lookup = FALSE
    ),
    c(base_word = "they", base_word = "are")
  )
  testthat::expect_identical(
    process_word(
      "would've",
      use_lookup = FALSE
    ),
    c(base_word = "would", base_word = "have")
  )
  testthat::expect_identical(
    process_word(
      "wouldn't've",
      use_lookup = FALSE
    ),
    c(base_word = "would", base_word = "not", base_word = "have")
  )
  testthat::expect_identical(
    process_word(
      "'dillo",
      use_lookup = FALSE
    ),
    c(base_word = "armadillo")
  )

  # Let's also deal with misspellings that are common enough to have wiktionary
  # pages.
  testthat::expect_identical(
    process_word(
      "mispelling",
      use_lookup = FALSE
    ),
    process_word(
      "misspelling",
      use_lookup = FALSE
    )
  )
  testthat::expect_identical(
    process_word(
      "'twas",
      use_lookup = FALSE
    ),
    c(base_word = "it", base_word = "was")
  )

  # Deal with deep links.
  testthat::expect_identical(
    process_word(
      "lobbying",
      use_lookup = FALSE
    ),
    c(base_word = "lobby", inflection = "-ing")
  )

  # Don't follow a bad rabbit trail for some words.
  testthat::expect_identical(
    process_word(
      "prejudice",
      use_lookup = FALSE
    ),
    c(base_word = "prejudice")
  )

  # Also test some other corner cases.
  testthat::expect_identical(
    process_word(
      "-'ve",
      use_lookup = FALSE
    ),
    c(base_word = "have")
  )

  testthat::expect_identical(
    process_word(
      "butchers'",
      use_lookup = FALSE
    ),
    c(base_word = "butcher", inflection = "-s", suffix = "-'s")
  )
  testthat::expect_identical(
    process_word(
      "butcher's",
      use_lookup = FALSE
    ),
    c(base_word = "butcher", suffix = "-'s")
  )
  testthat::expect_identical(
    process_word(
      "Addis Ababa",
      use_lookup = FALSE
    ),
    c(base_word = "Addis", base_word = "Ababa")
  )
  testthat::expect_identical(
    process_word(
      "a-flat",
      use_lookup = FALSE
    ),
    c(base_word = "A", base_word = "flat")
  )
})

test_that("process_word deals with fake words gracefully.", {
  word <- "sdjklf"
  test_result <- process_word(word, use_lookup = FALSE)
  expect_identical(test_result, rlang::set_names(word, "base_word"))
  test_result <- process_word(word, use_lookup = TRUE)
  expect_identical(test_result, rlang::set_names(word, "base_word"))
})

test_that("process_word and its children deal with weird cases.", {
  # I can't find a case where this actually happens, but I wanted to protect
  # against it.
  expect_identical(
    .split_on_breaks(c(suffix = "-this-that-")),
    c(
      suffix = "-this",
      suffix = "that-"
    )
  )
})

test_that("Template abuse doesn't kill us.", {
  testthat::expect_identical(
    .split_prefixes_wt("===Etymology===\n{{prefix|en|hemi|demi|semi|quaver}}"),
    c(
      prefix = "hemi-",
      prefix = "demi-",
      prefix = "semi-",
      base_word = "quaver"
    )
  )
  testthat::expect_identical(
    .split_prefixes_wt("===Etymology===\n{{prefix|en|hemi|demi|semi-}}"),
    c(
      prefix = "hemi-",
      prefix = "demi-",
      prefix = "semi-"
    )
  )
  testthat::expect_identical(
    .split_suffixes_wt("===Etymology===\n{{suffix|en|quave|er|est}}"),
    c(
      base_word = "quave",
      suffix = "-er",
      suffix = "-est"
    )
  )
})

test_that("process_word gives warnings for weird cases.", {
  expect_warning(
    process_word(c("two", "words")),
    "More than one word"
  )
})

test_that("Can use a lookup other than the default.", {
  # Before saving the fake lookup, make sure a broken cache properly returns as
  # NULL.
  old_option <- getOption("wikimorphemes.dir")
  on.exit(
    options(wikimorphemes.dir = old_option)
  )
  set_wikimorphemes_cache_dir(tempdir())

  memoise::forget(.cache_lookup)
  reset_lookup()
  test_result <- .cache_lookup()
  expect_null(test_result)

  fake_lookup <- dplyr::tibble(
    word = "upendings",
    morphemes = list(
      c(prefix = "down", base_word = "start")
    ),
    n_morphemes = 2L,
    timestamp = lubridate::now()
  )

  filename <- fs::path(
    tempdir(),
    "wikimorphemes",
    ext = "rds"
  )

  saveRDS(
    fake_lookup,
    filename
  )
  on.exit(
    {
      unlink(filename)
      memoise::forget(.cache_lookup)
    },
    add = TRUE
  )

  # Now we need to force a reload of the cache.
  memoise::forget(.cache_lookup)
  memoise::forget(.populate_env_lookup)

  testthat::expect_identical(
    process_word(word = "upendings"),
    c(prefix = "down", base_word = "start")
  )
})
