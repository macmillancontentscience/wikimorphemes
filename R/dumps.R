cache_dir <- function(dir = NULL) {
  return(
    dir %||%
      getOption("wikimorphemes.dir") %||%
      rappdirs::user_cache_dir(appname = "wikimorphemes")
  )
}
