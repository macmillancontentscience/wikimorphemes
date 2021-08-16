# onload ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  set_wikimorphemes_cache_dir() #nocov start
  .cache_wikitext <<- memoise::memoise(.cache_wikitext)
  .cache_lookup <<- memoise::memoise(.cache_lookup)
  .populate_env_lookup <<- memoise::memoise(.populate_env_lookup) # nocov end
}
