% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dumps.R
\name{.create_cache_files}
\alias{.create_cache_files}
\title{Download the Latest Dump and Create Lookup}
\usage{
.create_cache_files(sight_words = default_sight_words())
}
\arguments{
\item{sight_words}{Character vector; words to \emph{not} break down further.
Defaults to the included \code{sight_words} list; to include no sight words,
pass in an empty character vector to this parameter.}
}
\value{
A logical scalar (invisibly) indicating whether the data was updated.
}
\description{
This function wraps \code{\link{.create_wikitext_en}} and
\code{\link{.create_lookup}} to download the latest wiktionary dump and
process it into a lookup.
}
\keyword{internal}
