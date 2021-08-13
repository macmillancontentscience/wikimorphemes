
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wikimorphemes

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of wikimorphemes is to provide tools for extracting morphemes
from Wiktionary entries. The wikimorphemes api is *not yet stable* and
may change (most notably to provide a vectorized processor).

## Installation

You can install the released version of wikimorphemes from
[CRAN](https://CRAN.R-project.org) with:

``` r
# No you can't.
#install.packages("wikimorphemes")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jonthegeek/wikimorphemes")
```

## Examples

The main function in wikimorphemes is `process_word`.

``` r
library(wikimorphemes)
process_word("unaffable")
#>    prefix base_word 
#>     "un-" "affable"
process_word("understandable")
#>    base_word       suffix 
#> "understand"      "-able"
process_word("standing")
#>  base_word inflection 
#>    "stand"     "-ing"
```

## Contributing to {wikimorphemes}

If you are going to work on this package, please begin by running
`download_wikimorphemes_lookup()`. This will download the latest cached
version of the lookup (10.7MB), which will then be used in tests.

## Code of Conduct

Please note that the wikimorphemes project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
