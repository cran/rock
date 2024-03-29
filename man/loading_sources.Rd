% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_source.R, R/load_sources.R
\name{load_source}
\alias{load_source}
\alias{load_sources}
\alias{loading_sources}
\title{Load a source from a file or a string}
\usage{
load_source(
  input,
  encoding = rock::opts$get("encoding"),
  silent = rock::opts$get("silent"),
  rlWarn = rock::opts$get(rlWarn),
  diligentWarnings = rock::opts$get("diligentWarnings")
)

load_sources(
  input,
  filenameRegex = ".*",
  ignoreRegex = NULL,
  recursive = TRUE,
  full.names = FALSE,
  encoding = rock::opts$get("encoding"),
  silent = rock::opts$get("silent")
)
}
\arguments{
\item{input}{The filename or contents of the source
for \code{load_source} and the directory containing the
sources for \code{load_sources}.}

\item{encoding}{The encoding of the file(s).}

\item{silent}{Whether to be chatty or quiet.}

\item{rlWarn}{Whether to let \code{\link[=readLines]{readLines()}} warn, e.g. if files do not end
with a newline character.}

\item{diligentWarnings}{Whether to display very diligent warnings.}

\item{filenameRegex}{A regular expression to match against located files; only
files matching this regular expression are processed.}

\item{ignoreRegex}{Regular expression indicating which files to ignore. This
is a perl-style regular expression (see \link[base:regex]{base::regex}).}

\item{recursive}{Whether to search all subdirectories (\code{TRUE}) as well or not.}

\item{full.names}{Whether to store source names as filenames only or whether
to include paths.}
}
\value{
Invisibly, an R character vector of
classes \code{rock_source} and \code{character}.
}
\description{
These functions load one or more source(s) from a file or
a string and store it in memory for further processing.
Note that you'll probably want to clean the sources
first, using one of the \code{\link[=clean_sources]{clean_sources()}} functions,
and you'll probably want to add utterance identifiers
to each utterance using one of the \code{\link[=prepending_uids]{prepending_uids()}}
functions.
}
\examples{
### Get path to example source
examplePath <-
  system.file("extdata", package="rock");

### Get a path to one example file
exampleFile <-
  file.path(examplePath, "example-1.rock");

### Parse single example source
loadedSource <- rock::load_source(exampleFile);
}
