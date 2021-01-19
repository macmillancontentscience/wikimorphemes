## code to prepare `sysdata.R` dataset goes here

.baseword_name <- "base_word"
.inflection_name <- "inflection"
.prefix_name <- "prefix"
.suffix_name <- "suffix"
.interfix_name <- "interfix"

usethis::use_data(.baseword_name,
                  .inflection_name,
                  .prefix_name,
                  .suffix_name,
                  .interfix_name,
                  internal = TRUE, overwrite = TRUE)
rm(.baseword_name)
rm(.inflection_name)
rm(.prefix_name)
rm(.suffix_name)
rm(.interfix_name)
