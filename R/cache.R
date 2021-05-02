.cache_wikitext <- function() {
  cache_file <- fs::path(
    rappdirs::user_cache_dir(appname = "wikimorphemes"),
    "wikitext_en",
    ext = "rds"
  )
  if (file.exists(cache_file)) {
    return(
      readRDS(cache_file)
    )
  }
  else {
    return(NULL)
  }
}
