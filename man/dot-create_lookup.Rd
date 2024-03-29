% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dumps.R
\name{.create_lookup}
\alias{.create_lookup}
\title{Generate the Lookup for All Words in Dump}
\usage{
.create_lookup(sight_words = default_sight_words())
}
\arguments{
\item{sight_words}{Character vector; words to \emph{not} break down further.
Defaults to the included \code{sight_words} list; to include no sight words,
pass in an empty character vector to this parameter.}
}
\value{
TRUE (invisibly), via save_wikimorphemes_lookup on.exit.
}
\description{
The wikitext dump contains information about all words on wiktionary. This
function runs each of those words through process_word, generating a new
lookup in the process. The lookup is then saved to disk.
}
\keyword{internal}
