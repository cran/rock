#' Load a source from a file or a string
#'
#' These functions load one or more source(s) from a file or
#' a string and store it in memory for further processing.
#' Note that you'll probably want to clean the sources
#' first, using one of the [clean_sources()] functions,
#' and you'll probably want to add utterance identifiers
#' to each utterance using one of the [prepending_uids()]
#' functions.
#'
#' @param input The filename or contents of the source
#' for `load_source` and the directory containing the
#' sources for `load_sources`.
#' @param encoding The encoding of the file(s).
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, an R character vector of
#' classes `rock_source` and `character`.
#' @rdname loading_sources
#' @export
load_source <- function(input,
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

  class(res) <- c("rock_source", "character");

  return(invisible(res));

}
