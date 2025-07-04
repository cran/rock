% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heading_vector.R
\name{heading_vector}
\alias{heading_vector}
\title{Make a vector of strings into headings}
\usage{
heading_vector(
  x,
  headingLevel = rock::opts$get("defaultHeadingLevel"),
  output = "markdown",
  cat = FALSE
)
}
\arguments{
\item{x}{The vector.}

\item{headingLevel}{The level of the heading; the default can be set
with e.g. \code{rock::opts$set(defaultHeadingLevel=1)}.}

\item{output}{Whether to output to HTML ("\code{html}") or markdown (anything
else).}

\item{cat}{Whether to cat (print) the heading or just invisibly return it.}
}
\value{
The heading, invisibly.
}
\description{
This is just a convenience function to convert a vector of strings into
markdown or HTML headings at a given 'depth'.
}
\examples{
rock::heading_vector(c("Hello ", "World"), headingLevel=5);
### This produces: "\\n\\n##### Hello\\n\\n" and
### "\\n\\n##### World\\n\\n"
}
