#' @rdname cleaning_sources
#' @param recursive Whether to search all subdirectories (`TRUE`) as well or not.
#' @param filenameRegex A regular expression to match against located files; only
#' files matching this regular expression are processed.
#' @param filenamePrefix,filenameSuffix The prefix and suffix to add to the
#' filenames when writing the processed files to disk.
#' @export
clean_sources <- function(input,
                          output,
                          filenamePrefix = "",
                          filenameSuffix = "",
                          recursive=TRUE,
                          filenameRegex=".*",
                          replacementsPre = rock::opts$get(replacementsPre),
                          replacementsPost = rock::opts$get(replacementsPost),
                          extraReplacementsPre = NULL,
                          extraReplacementsPost = NULL,
                          removeNewlines = FALSE,
                          utteranceSplits = rock::opts$get(utteranceSplits),
                          preventOverwriting = rock::opts$get(preventOverwriting),
                          encoding = rock::opts$get(encoding),
                          silent=rock::opts$get(silent)) {

  utteranceMarker <- rock::opts$get(utteranceMarker);

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

  if (any(grepl("\\.rock$",
                rawSourceFiles))) {
    if ((nchar(filenamePrefix) == 0) && (nchar(filenameSuffix) == 0)) {
      stop("At least one of the input files already has the .rock extension! ",
           "Therefore, you have to provide at least one of `filenamePrefix` and `filenameSuffix` ",
           "to allow saving the files to new names!");
    }
  }

  res <- character();
  for (filename in rawSourceFiles) {
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
    clean_source(input = filename,
                 output = file.path(newFileDir,
                                    newFilename),
                 replacementsPre=replacementsPre,
                 extraReplacementsPre=extraReplacementsPre,
                 utteranceSplits=utteranceSplits,
                 replacementsPost=replacementsPost,
                 extraReplacementsPost=extraReplacementsPost,
                 preventOverwriting=preventOverwriting,
                 removeNewlines=removeNewlines,
                 encoding=encoding,
                 silent=silent);
    res <-
      c(res,
        newFilename);
  }
  if (!silent) {
    message("I just wrote ", length(rawSourceFiles), " cleaned sources to path '",
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
            "the raw sources; 'clean-sources' for the cleaned sources (which ",
            "should then be the `output` specified to this `clean_sources` ",
            "function), and 'coded-sources' for the coded sources. If you have ",
            "multiple coders, use e.g. 'coded-sources-coder-A' and ",
            "'coded-sources-coder-B' to organise these versions.");
  }
  invisible(res);
}
