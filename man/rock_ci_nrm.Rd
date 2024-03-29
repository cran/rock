% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_import_nrm_spec.R
\name{ci_import_nrm_spec}
\alias{ci_import_nrm_spec}
\alias{print.rock_ci_nrm}
\title{Import a Narrative Response Model specification}
\usage{
ci_import_nrm_spec(
  x,
  read_ss_args = list(exportGoogleSheet = TRUE),
  defaultLanguage = NULL,
  silent = rock::opts$get("silent")
)

\method{print}{rock_ci_nrm}(x, ...)
}
\arguments{
\item{x}{A path to a file or an URL to a Google Sheet, passed
to \code{\link[=read_spreadsheet]{read_spreadsheet()}}.}

\item{read_ss_args}{A named list with arguments to pass to
\code{\link[=read_spreadsheet]{read_spreadsheet()}}.}

\item{defaultLanguage}{Language to set as default language (by default,
i.e. if \code{NULL}, the first language is used).}

\item{silent}{Whether to be silent or chatty.}

\item{...}{Additional arguments are ignored.}
}
\value{
A \code{rock_ci_nrm} object.
}
\description{
Narrative Response Models are a description of the theory of how a
measurement instrument that measures a psychological construct works,
geared towards conducting cognitive interviews to verify the validity
of that measurement instrument. One a Narrative Response Model has been
imported, it can be used to generate interview schemes, overview of each
item's narrative response model, and combined with coded cognitive
interview notes or transcripts.
}
