% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codebook_fromSpreadsheet.R
\name{codebook_fromSpreadsheet}
\alias{codebook_fromSpreadsheet}
\title{Import a code book specification from a spreadsheet}
\usage{
codebook_fromSpreadsheet(
  x,
  localBackup = NULL,
  exportGoogleSheet = TRUE,
  xlsxPkg = c("rw_xl", "openxlsx", "XLConnect"),
  silent = rock::opts$get("silent")
)
}
\arguments{
\item{x}{The URL or path to a file.}

\item{localBackup}{If not \code{NULL}, a valid filename to write a local
backup to.}

\item{exportGoogleSheet}{If \code{x} is a URL to a Google Sheet, instead of using
the \code{googlesheets4} package to download the data, by passing
\code{exportGoogleSheet=TRUE}, an export link will be produced and the data
will be downloaded as Excel spreadsheet.}

\item{xlsxPkg}{Which package to use to work with Excel spreadsheets.}

\item{silent}{Whether to be silent or chatty.}
}
\value{
The code book specification as a \code{rock} code book object
}
\description{
Import a code book specification from a spreadsheet
}
\examples{
### This requires an active internet connection
if (FALSE) {
  gs_url <- paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1gVx5uhYzqcTH6Jq7AYmsLvHSBaYaT-23c7ZhZF4jmps"
  );
  codebook <- rock::codebook_fromSpreadsheet(gs_url);
}
}
