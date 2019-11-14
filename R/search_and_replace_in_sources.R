#' @rdname cleaning_sources
#' @export
search_and_replace_in_sources <- function(input,
                                          output,
                                          replacements = NULL,
                                          filenamePrefix = "",
                                          filenameSuffix = "_postReplacing",
                                          preventOverwriting = rock::opts$get(preventOverwriting),
                                          recursive=TRUE,
                                          filenameRegex=".*",
                                          encoding = rock::opts$get(encoding),
                                          silent=FALSE) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!is.character(output) || !length(output)==1) {
    stop("Only specify a single string as 'output'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (!(tolower(output) == "same")) {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  if ((nchar(filenamePrefix) == 0) && (nchar(filenameSuffix) == 0)) {
    stop("You have to provide at least one of `filenamePrefix` and `filenameSuffix` ",
         "to allow saving the files to new names!");
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE,
               pattern = filenameRegex,
               recursive=recursive);

  ### Delete directories, if any were present
  rawSourceFiles <-
    setdiff(rawSourceFiles,
            list.dirs(input,
                      full.names=TRUE));

  res <- character();
  for (filename in rawSourceFiles) {
    if (((nchar(filenamePrefix) > 0) &&
          grepl(filenamePrefix,
                basename(filename))) ||
        ((nchar(filenameSuffix) > 0) &&
          grepl(filenameSuffix,
                basename(filename)))) {
      if (!silent) {
        message("File '", basename(filename), "' already contains ",
                "the prefix ('", filenamePrefix, "') or suffix ('",
                filenameSuffix, "') string; skipping this file!");
      }
    } else {
      newFilename <-
        paste0(filenamePrefix,
               sub("^(.*)\\.[a-zA-Z0-9]+$",
                   "\\1",
                   basename(filename)),
               filenameSuffix,
               ".rock");
      if (tolower(output) == "same") {
        newFileDir <-
          dirname(filename);
      } else {
        newFileDir <-
          output;
      }
      search_and_replace_in_source(input = filename,
                                   output = file.path(newFileDir,
                                                      newFilename),
                                   replacements=replacements,
                                   preventOverwriting=preventOverwriting,
                                   encoding=encoding,
                                   silent=TRUE);
      res <-
        c(res,
          newFilename);
    }
  }
  if (!silent) {
    message("I just wrote ", length(rawSourceFiles), " 'post-search-replace-sources' to path '",
            output,
            "' ",
            ifelse(preventOverwriting,
                   "(unless the files already existed)",
                   "(overwriting any files that may already have existed)"),
            ". Note that these files may all be overwritten if this ",
            "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
            "Therefore, make sure to copy them to ",
            "another directory before starting to code those sources!\n\n",
            "A recommended convention is to place all data in a directory ",
            "called 'data', and use three subdirectories: 'raw-sources' for ",
            "the raw sources; 'clean-sources' for the cleaned sources, ",
            "and 'coded-sources' for the coded sources. If you have ",
            "multiple coders, use e.g. 'coded-sources-coder-A' and ",
            "'coded-sources-coder-B' to organise these versions, or use ",
            "different filenames (and use the coderId).");
  }
  invisible(res);
}
