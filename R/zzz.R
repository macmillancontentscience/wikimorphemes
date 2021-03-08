
# onload ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  split_inflections <<- memoise::memoise(split_inflections) # nocov
  split_morphemes <<- memoise::memoise(split_morphemes) # nocov
}
