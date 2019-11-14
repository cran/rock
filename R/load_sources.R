#' @rdname loading_sources
#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @param ignoreRegex Regular expression indicating which files to ignore.
#' @param full.names Whether to store source names as filenames only or whether
#' to include paths.
#'
#' @export
load_sources <- function(input,
                         encoding="UTF-8",
                         filenameRegex=".*",
                         ignoreRegex=NULL,
                         recursive=TRUE,
                         full.names=FALSE,
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
               pattern=filenameRegex,
               recursive=recursive,
               full.names=TRUE);

  if (!is.null(ignoreRegex)) {
    rawSourceFiles <-
      rawSourceFiles[!grepl(ignoreRegex,
                            rawSourceFiles,
                            perl=TRUE)];
  }

  res <- list();
  for (filename in rawSourceFiles) {
    fileNameToUse <-
      ifelse(full.names,
             filename,
             basename(filename));

    res[[fileNameToUse]] <-
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
