% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dumps.R
\name{.generate_cache_write_filename}
\alias{.generate_cache_write_filename}
\title{Generate Filenames for Wikitext Cache Files}
\usage{
.generate_cache_write_filename(filename, ext = "rds")
}
\arguments{
\item{filename}{Character scalar; the filename minus the extension, such as
"wikitext_en".}

\item{ext}{Character scalar; the file extension (default "rds").}
}
\value{
A character scalar validated path for writing.
}
\description{
Generate Filenames for Wikitext Cache Files
}
\keyword{internal}
