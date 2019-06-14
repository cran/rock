#' Prepending unique utterance identifiers
#'
#' This function prepending unique utterance identifiers to each
#' utterance (line) in a source. Note that you'll probably want
#' to clean the sources using [clean_sources()] first.
#'
#' @param input The filename or contents of the source
#' for `prepend_ids_to_source` and the directory containing the
#' sources for `prepend_ids_to_sources`.
#' @param output The filename where to write the resulting file for
#' `prepend_ids_to_source` and the directory where to write the
#' resulting files for `prepend_ids_to_sources`
#' @param origin The time to use for the first identifier.
#' @param preventOverwriting Whether to overwrite existing files (`FALSE`)
#' or prevent that from happening (`TRUE`).
#' @param encoding The encoding of the file(s).
#' @param silent Whether to be chatty or quiet.
#'
#' @return The source with prepended uids, either invisible (if `output`
#' if specified) or visibly (if not).
#' @aliases prepending_uids
#' @rdname prepending_uids
#' @examples prepend_ids_to_source(input = "brief\nexample\nsource");
#' @export
prepend_ids_to_source <- function(input,
                                  output = NULL,
                                  origin=Sys.time(),
                                  preventOverwriting=TRUE,
                                  encoding="UTF-8",
                                  silent=FALSE) {
  if (file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding);
  } else {
    res <- input;
    if ((length(res) == 1) && grepl('\n', res)) {
      res <-
        strsplit(res,
                 "\n")[[1]];
    }
  }

  uids <-
    generate_uids(length(res),
                  origin=origin);

  res <- paste0(uids, " ", res);

  if (is.null(output)) {
    return(res);
  } else {
    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    if (file.exists(output) && preventOverwriting) {
      if (!silent) {
        message("File '",
                output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                "write the source with prepended utterance identifiers (uids to ",
                "disk.");
      }
    } else {
      con <- file(description=output,
                  open="w",
                  encoding=encoding);
      writeLines(text=res,
                 con=con);
      close(con);
    }
    if (!silent) {
      message("I just wrote a file with a source with prepended utterance identifiers (uids) to '",
              output,
              "'. Note that this file may be overwritten if this ",
              "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
              "Therefore, make sure to copy it to ",
              "another directory, or rename it, before starting to code this source!");
    }
    invisible(res);
  }

}
