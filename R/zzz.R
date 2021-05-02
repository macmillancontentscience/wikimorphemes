
# onload ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  split_inflections <<- memoise::memoise(split_inflections) # nocov
  split_morphemes <<- memoise::memoise(split_morphemes) # nocov
  .cache_wikitext <<- memoise::memoise(.cache_wikitext) # nocov
}
