% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_source.R, R/write_sources.R
\name{write_source}
\alias{write_source}
\alias{write_sources}
\alias{writing_sources}
\title{Write a source to a file}
\usage{
write_source(
  x,
  output,
  encoding = rock::opts$get("encoding"),
  preventOverwriting = rock::opts$get("preventOverwriting"),
  silent = rock::opts$get("silent")
)

write_sources(
  x,
  output,
  filenamePrefix = "",
  filenameSuffix = "_written",
  recursive = TRUE,
  encoding = rock::opts$get("encoding"),
  preventOverwriting = rock::opts$get("preventOverwriting"),
  silent = rock::opts$get("silent")
)
}
\arguments{
\item{x}{The source(s).}

\item{output}{The filename (for \code{rock::write_source()}) or path (for
\code{rock::write_sources()}) to write to.}

\item{encoding}{The encoding to use.}

\item{preventOverwriting}{Whether to prevent against overwriting of the
file(s) to write. Set to \code{FALSE} to overwrite.}

\item{silent}{Whether to be chatty or quiet.}

\item{filenamePrefix, filenameSuffix}{Optional prefixes or suffixes to pre-
or append to the filenames when writing the files.}

\item{recursive}{Whether to recursively create directories if the \code{output}
directory does not yet exist.}
}
\value{
Invisibly, the input (\code{x}), to enable chaining in pipes.
}
\description{
These functions write one or more source(s) from memory (as
loaded by \code{\link[=load_source]{load_source()}} or \code{\link[=load_sources]{load_sources()}} to a file.
}
\examples{
### Get path to example source
examplePath <-
  system.file("extdata", package="rock");

### Get a path to one example file
exampleFile <-
  file.path(examplePath, "example-1.rock");

### Get a temporary file to write to
tempFile <- tempfile(fileext = ".rock")

### For R versions below 4.1
loadedSource <-
  rock::load_source(exampleFile);

loadedSource <-
  rock::code_source(
    loadedSource,
    c("Lorem Ipsum" = "lorumIpsum")
  );

rock::write_source(
  loadedSource,
  tempFile
);

### From R 4.1 onwards, you can also chain
### these commands using the pipe operator.
###
### Note that that means that this example
### will not run if you have a previous
### version of R.
loadedSource <-

  rock::load_source(exampleFile) |>

  rock::code_source(c("Lorem Ipsum" = "lorumIpsum")) |>

  rock::write_source(tempFile);

}
