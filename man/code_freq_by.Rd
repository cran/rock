% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_freq_by.R
\name{code_freq_by}
\alias{code_freq_by}
\title{Code frequencies separate by a variable}
\usage{
code_freq_by(x, by, codes = ".*", returnTidyDf = FALSE)
}
\arguments{
\item{x}{The object with parsed sources.}

\item{by}{The variables on which to split when computing code frequencies.}

\item{codes}{A regular expression specifying the codes fo which to compute
the code frequencies.}

\item{returnTidyDf}{When \code{TRUE}, return a tidy data frame with the counts
in one column, the \code{by} variable in another, and the code for which the
counts are provided in another column. Otherwise, return a 'wide' data
frame with the \code{by} variable in one column, the codes in the other columns,
and the counts in the cells.}
}
\value{
A data frame with the code frequencies
}
\description{
Code frequencies separate by a variable
}
\examples{
### Get path to example source
examplePath <-
  system.file("extdata", package="rock");

### Get a path to one example file
exampleFile <-
  file.path(examplePath, "example-1.rock");

### Load example source
loadedExample <- rock::parse_source(exampleFile);

### Show code frequencies
code_freq_by(loadedExample, "nestingLevel");
}
