#' @rdname loading_sources
#' @export
load_sources <- function(input,
                         encoding="UTF-8",
                         silent=FALSE) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE);

  res <- list();
  for (filename in rawSourceFiles) {
    res[[basename(filename)]] <-
      load_source(filename,
                  encoding=encoding,
                  silent=TRUE);
  }

  if (!silent) {
    message("I just loaded ", length(rawSourceFiles), " sources.");
  }

  class(res) <-
    "rock_loaded_sources_list";

  invisible(res);
}
