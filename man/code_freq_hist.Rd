% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_freq_hist.R
\name{code_freq_hist}
\alias{code_freq_hist}
\title{Create a frequency histogram for codes}
\usage{
code_freq_hist(
  x,
  codes = ".*",
  sortByFreq = "decreasing",
  forceRootStripping = FALSE,
  trimSourceIdentifiers = 20,
  ggplot2Theme = ggplot2::theme(legend.position = "bottom"),
  silent = rock::opts$get("silent")
)
}
\arguments{
\item{x}{A parsed source(s) object.}

\item{codes}{A regular expression to select codes to include.}

\item{sortByFreq}{Whether to sort by frequency decreasingly
(\code{decreasing}, the default), increasingly (\code{increasing}),
or alphabetically (\code{NULL}).}

\item{forceRootStripping}{Force the stripping of roots, even if they are
different.}

\item{trimSourceIdentifiers}{If not \code{NULL}, the number of character to trim
the source identifiers to.}

\item{ggplot2Theme}{Can be used to specify theme elements for the plot.}

\item{silent}{Whether to be chatty or silent.}
}
\value{
a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.
}
\description{
Create a frequency histogram for codes
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
code_freq_hist(loadedExample);
}
