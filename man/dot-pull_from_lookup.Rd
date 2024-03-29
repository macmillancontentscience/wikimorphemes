% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/env.R
\name{.pull_from_lookup}
\alias{.pull_from_lookup}
\title{Pull a Word from the Lookup}
\usage{
.pull_from_lookup(word)
}
\arguments{
\item{word}{Character; a word to process.}
}
\value{
A character vector of morphemes, or a length-0 character vector if no
morphemes are found.
}
\description{
This is in a separate function because it's an area that we might be able to
optimize. I tried memoising, but that seemed to fight with and confuse the
cache (made things way slower).
}
\keyword{internal}
