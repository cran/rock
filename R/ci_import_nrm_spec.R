#' Import a Narrative Response Model specification
#'
#' Narrative Response Models are a description of the theory of how a
#' measurement instrument that measures a psychological construct works,
#' geared towards conducting cognitive interviews to verify the validity
#' of that measurement instrument. One a Narrative Response Model has been
#' imported, it can be used to generate interview schemes, overview of each
#' item's narrative response model, and combined with coded cognitive
#' interview notes or transcripts.
#'
#' @param x A path to a file or an URL to a Google Sheet, passed
#' to [rock::read_spreadsheet()].
#' @param read_ss_args A named list with arguments to pass to
#' [rock::read_spreadsheet()].
#' @param silent Whether to be silent or chatty.
#' @param ... Additional arguments are ignored.
#'
#' @return A `rock_ci_nrm` object.
#' @rdname rock_ci_nrm
#' @export
ci_import_nrm_spec <- function(x,
                               read_ss_args=list(exportGoogleSheet = TRUE),
                               silent = rock::opts$get("silent")) {

  nrm_wsNames <- rock::opts$get("nrm_wsNames");
  nrm_colNames <- rock::opts$get("nrm_colNames");

  if (is.null(read_ss_args)) {
    read_ss_args <- list(x = x);
  } else {
    read_ss_args <- c(list(x = x),
                      read_ss_args);
  }

  nrm_spec <-
    do.call(
      read_spreadsheet,
      read_ss_args
    );

  if (nrm_wsNames$metadata %in% names(nrm_spec)) {
    nrm_spec[[nrm_wsNames$metadata]] <-
      stats::setNames(
        as.list(
          nrm_spec[[nrm_wsNames$metadata]][, nrm_colNames$metadata['metadata_content']]
        ),
        nm = nrm_spec[[nrm_wsNames$metadata]][, nrm_colNames$metadata['metadata_field']]
      );
  }

  class(nrm_spec) <- "rock_nrm_spec";

  res <- list(nrm_spec = nrm_spec);

  languages <-
    unique(
      nrm_spec[[nrm_wsNames$stimuli]][[nrm_colNames$stimuli['stimulus_language']]]
    );

  res$interviewSchemes <- list();
  res$nrm_md <- list();
  res$nrm_with_probes_md <- list();

  for (currentLanguage in languages) {

    res$interviewSchemes[[currentLanguage]] <-
      ci_create_interviewScheme(
        nrm_spec = nrm_spec,
        language = currentLanguage
      );

    res$nrm_md[[currentLanguage]] <-
      ci_nrm_to_md(
        nrm_spec = nrm_spec,
        language = currentLanguage
      );

    res$nrm_with_probes_md[[currentLanguage]] <-
      ci_nrm_to_md(
        nrm_spec = nrm_spec,
        language = currentLanguage,
        includeProbes = TRUE
      );

  }

  class(res) <- "rock_ci_nrm";

  return(res);

}

#' @rdname rock_ci_nrm
#' @export
#' @method print rock_ci_nrm
print.rock_ci_nrm <- function(x, ...) {

  cat("Narrative Response Model Specification for Cognitive Interviews\n\n");

  cat0("This specification contains ", nrow(x$nrm_spec$instrument), " items.\n");

  return(invisible(x));

}