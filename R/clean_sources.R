#' @rdname cleaning_sources
#' @export
clean_sources <- function(input,
                          output,
                          replacementsPre = list(c("([^\\.])(\\.\\.)([^\\.])",
                                                   "\\1.\\3"),
                                                 c("([^\\.])(\\.\\.\\.\\.+)([^\\.])",
                                                   "\\1...\\3"),
                                                 c("(\\s*\\r?\\n){3,}",
                                                   "\n")),
                          extraReplacementsPre = NULL,
                          utteranceSplits = c("([\\?\\!]+\\s?|\u2026\\s?|[[:alnum:]\\s?]\\.(?!\\.\\.)\\s?)"),
                          utteranceMarker = "\n",
                          replacementsPost = list(c("([^\\,]),([^\\s])",
                                                    "\\1, \\2")),
                          extraReplacementsPost = NULL,
                          preventOverwriting=TRUE,
                          removeNewlines = FALSE,
                          encoding = "UTF-8",
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

  if (!dir.exists(output)) {
    warning("Directory provided to write to ('",
            output,
            "') does not exist - creating it!");
    dir.create(output,
               recursive = TRUE);
  }

  rawSourceFiles <-
    list.files(input,
               full.names=TRUE);

  res <- character();
  for (filename in rawSourceFiles) {
    newFilename <-
      paste0(sub("^(.*)\\.[a-zA-Z0-9]+$",
                 "\\1",
                 basename(filename)),
             ".rock");
    clean_source(input = filename,
                 output = file.path(output,
                                    newFilename),
                 replacementsPre=replacementsPre,
                 extraReplacementsPre=extraReplacementsPre,
                 utteranceSplits=utteranceSplits,
                 utteranceMarker=utteranceMarker,
                 replacementsPost=replacementsPost,
                 extraReplacementsPost=extraReplacementsPost,
                 preventOverwriting=preventOverwriting,
                 removeNewlines=removeNewlines,
                 encoding=encoding,
                 silent=TRUE);
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
