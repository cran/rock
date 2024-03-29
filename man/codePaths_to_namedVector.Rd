% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codePaths_to_namedVector.R
\name{codePaths_to_namedVector}
\alias{codePaths_to_namedVector}
\title{Get a vector to find the full paths based on the leaf code identifier}
\usage{
codePaths_to_namedVector(x)
}
\arguments{
\item{x}{A vector of code paths.}
}
\value{
The named vector of code paths.
}
\description{
This function names a vector with the leaf code
using the \code{codeTreeMarker} stored in the \link{opts} object as marker.
}
\examples{
codePaths_to_namedVector(
  c("codes>reason>parent_feels",
    "codes>reason>child_feels")
);
}
