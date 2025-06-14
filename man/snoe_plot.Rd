% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snoe_plot.R
\name{snoe_plot}
\alias{snoe_plot}
\title{Soft Non-numeric Occurrence Estimation (SNOE) plot}
\usage{
snoe_plot(
  x,
  codes = ".*",
  matchRegexAgainstPaths = TRUE,
  estimateWithin = NULL,
  title = "SNOE plot",
  ggplot2Theme = ggplot2::theme_minimal(),
  greyScale = FALSE,
  colors = c("#0072B2", "#C0C0C0"),
  greyScaleColors = c("#808080", "#C0C0C0"),
  silent = rock::opts$get("silent")
)
}
\arguments{
\item{x}{A parsed source(s) object.}

\item{codes}{A regular expression to select codes to include, or,
alternatively, a character vector with literal code idenfitiers.}

\item{matchRegexAgainstPaths}{Whether to match the \code{codes} regular expression
against the full code paths or only against the code identifier.}

\item{estimateWithin}{The column specifying within what to count.}

\item{title}{Title of the plot}

\item{ggplot2Theme}{Can be used to specify theme elements for the plot.}

\item{greyScale}{Whether to produce the plot in color (\code{FALSE}) or greyscale
(\code{TRUE}).}

\item{colors, greyScaleColors}{The (two) colors to use for the color and
greyscale versions of the SNOE plot.}

\item{silent}{Whether to be chatty or silent}
}
\value{
a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.
}
\description{
Soft Non-numeric Occurrence Estimation (SNOE) plot
}
\examples{
### Get path to example source
examplePath <-
  system.file("extdata", package="rock");

### Get a path to one example file
exampleFile <-
  file.path(examplePath, "example-3.rock");

### Load example source
loadedExample <- rock::parse_source(exampleFile);

### Show code occurrence estimates
rock::snoe_plot(
  loadedExample
);

### Load two example sources
loadedExamples <- rock::parse_sources(
  examplePath,
  regex = "example-[34].rock"
);

rock::snoe_plot(
  loadedExamples
);
}
