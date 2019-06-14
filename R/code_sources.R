#' @rdname coding_sources
#' @export
code_sources <- function(input,
                         codes,
                         silent=FALSE) {

  if (!("rock_loaded_sources_list" %in% class(input))) {
    stop("The sources specified in the `input` argument have to be ",
         "stored in an object of class `rock_loaded_sources_list`, ",
         "as produced by a call to `load_sources`. The object ",
         "you provided has class ",
         vecTxtQ(class(input)), ".");
  }

  sourceNames <-
    names(input);
  sourceClass <-
    class(input);

  res <- lapply(input,
                code_source,
                codes=codes,
                silent=TRUE);

  names(res) <-
    sourceNames;

  if (!silent) {
    message("I just coded ", length(res), " sources.");
  }

  class(res) <-
    sourceClass;

  invisible(res);

}
