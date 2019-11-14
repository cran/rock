#' Create an overview of coded fragments
#'
#' Collect all coded utterances and optionally add some context
#' (utterances before and utterances after) to create ann overview
#' of all coded fragments per code.
#'
#' By default, the output is optimized for inclusion in an R Markdown
#' document. To optimize output for the R console or a plain text
#' file, without any HTML codes, set `add_html_tags` to FALSE, and
#' potentially set `cleanUtterances` to only return the utterances,
#' without the codes.
#'
#' @param x The parsed source(s) as provided by `rock::parse_source`
#' or `rock::parse_sources`.
#' @param codes The regular expression that matches the codes to include
#' @param context How many utterances before and after the target
#' utterances to include in the fragments.
#' @param heading Optionally, a title to include in the output. The title
#' will be prefixed with `headingLevel` hashes (`#`), and the codes with
#' `headingLevel+1` hashes. If `NULL` (the default), a heading will be
#' generated that includes the collected codes if those are five or less.
#' If a character value is specified, that will be used. To omit a heading,
#' set to anything that is not `NULL` or a character vector (e.g. `FALSE`).
#' If no heading is used, the code prefix will be `headingLevel` hashes,
#' instead of `headingLevel+1` hashes.
#' @param headingLevel The number of hashes to insert before the headings.
#' @param add_html_tags Whether to add HTML tags to the result.
#' @param rawResult Whether to return the raw result, a list of the
#' fragments, or one character value in markdown format.
#' @param output Here, a path and filename can be provided where the
#' result will be written. If provided, the result will be returned
#' invisibly.
#' @param template The template to load; either the name of one
#' of the ROCK templates (currently, only 'default' is available), or
#' the path and filename of a CSS file.
#' @param cleanUtterances Whether to use the clean or the raw utterances
#' when constructing the fragments (the raw versions contain all codes). Note that
#' this should be set to `FALSE` to have `add_html_tags` be of the most use.
#' @param preventOverwriting Whether to prevent overwriting of output files.
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#'
#' @return Either a list of character vectors, or a single character value.
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Show organised coded fragments in Markdown
#' cat(collect_coded_fragments(parsedExample));
#'
#' ### Only for the codes containing 'Code2'
#' cat(collect_coded_fragments(parsedExample,
#'                             'Code2'));
#'
#' @export
collect_coded_fragments <- function(x,
                                    codes = ".*",
                                    context = 0,
                                    heading = NULL,
                                    headingLevel = 2,
                                    add_html_tags = TRUE,
                                    cleanUtterances = FALSE,
                                    output = NULL,
                                    template = "default",
                                    rawResult = FALSE,
                                    preventOverwriting = rock::opts$get(preventOverwriting),
                                    silent=rock::opts$get(silent)) {

  fragmentDelimiter <- rock::opts$get(fragmentDelimiter);
  utteranceGlue <- ifelse(add_html_tags, "\n", rock::opts$get(utteranceGlue));
  sourceFormatting <- rock::opts$get(sourceFormatting);
  codeHeadingFormatting <- rock::opts$get(codeHeadingFormatting);

  if (!("rockParsedSource" %in% class(x)) &&
      !("rockParsedSources" %in% class(x))) {
    stop(glue::glue("The object you provided (as argument `x`) has class '{vecTxtQ(class(x))}', ",
                    "but I can only process objects obtained by parsing one or more sources (with ",
                    "`rock::parse_source` or `rock::parse_sources`), which have class 'rockParsedSource' ",
                    "or 'rockParsedSources'."));
  }

  if ("rockParsedSource" %in% class(x)) {
    singleSource <- TRUE;
  } else {
    singleSource <- FALSE;
  }

  matchedCodes <- grep(codes,
                       x$convenience$codingLeaves,
                       value=TRUE);
  dat <- x$mergedSourceDf;

  matchedCodesPaths <-
    x$convenience$codingPaths[matchedCodes];

  if (!silent) {
    cat0("The regular expression passed in argument `codes` ('",
              codes, "') matches the following codings: ",
              vecTxtQ(matchedCodes), ".\n\n");
  }

  ### Get line numbers of the fragments to extract,
  ### get fragments, store them
  res <- lapply(matchedCodes,
                function(i) {
                  return(lapply(which(dat[, i] == 1),
                           function(center) {
                             indices <- seq(center - context,
                                            center + context);

                             ### Store indices corresponding source of this utterance
                             if (singleSource) {
                               sourceIndices <- c(1, nrow(dat));
                             } else {
                               sourceIndices <-
                                 which(dat[, 'originalSource'] == dat[center, 'originalSource']);
                             }

                             ### If this source is shorter than the number of lines requested,
                             ### simply send the complete source
                             if (length(sourceIndices) <= (1 + 2*context)) {
                               indices <- sourceIndices;
                             } else {
                               ### Shift forwards or backwards to make sure early or late
                               ### fragments don't exceed valid utterance (line) numbers
                               indices <- indices - min(0, (min(indices) - min(sourceIndices)));
                               indices <- indices - max(0, (max(indices) - max(sourceIndices)));
                             }

                             ### Get clean or raw utterances
                             if (cleanUtterances) {
                               res <- dat[indices, 'utterances_clean'];
                             } else {
                               res <- dat[indices, 'utterances_raw'];
                             }

                             ### Add html tags, if requested
                             if (add_html_tags) {
                               res <- paste0(rock::add_html_tags(res));
                             }

                             ### Collapse all utterances into one character value
                             res <- paste0(res,
                                           collapse=utteranceGlue);

                             ### Add the sources, if necessary
                             if ((!identical(sourceFormatting, FALSE)) && !singleSource) {
                               res <- paste0(sprintf(sourceFormatting, dat[center, 'originalSource']),
                                             res);
                             }

                             ### Return result
                             return(res);
                           }));
                });

  if (rawResult) {
    names(res) <-
      matchedCodes;
  } else {
    ### Set codePrefix based on whether a heading
    ### will be included
    if (is.null(heading)) {
      if (length(matchedCodes) > 5) {
        heading <-
          paste0(repStr("#", headingLevel), " ",
                 "Collected coded fragments with ",
                 context, " lines of context",
                 "\n\n");
      } else {
        heading <-
          paste0(repStr("#", headingLevel), " ",
                 "Collected coded fragments for codes ",
                 vecTxtQ(matchedCodes), " with ",
                 context, " lines of context",
                 "\n\n");
      }
      codePrefix <-
        paste0(repStr("#", headingLevel+1), " ");
    } else if (is.character(heading)) {
      heading <-
        paste0(repStr("#", headingLevel), " ",
               heading, "\n\n");
      codePrefix <-
        paste0(repStr("#", headingLevel+1), " ");
    } else {
      heading <- FALSE;
      codePrefix <-
        paste0(repStr("#", headingLevel), " ");
    }

    ### Combine all fragments within each code
    res <- lapply(res,
                  paste0,
                  collapse=fragmentDelimiter);
    ### Unlist into vector
    res <- unlist(res);
    ### Add titles
    res <- paste0(codePrefix,
                  sprintf(codeHeadingFormatting, matchedCodes, matchedCodesPaths),
                  fragmentDelimiter,
                  res,
                  fragmentDelimiter);
    ### Collapse into one character value
    res <- paste0(res, collapse="\n");
    ### Add title heading
    if (!identical(heading, FALSE)) {
      res <- paste0(heading,
                    res);
    }
  }

  ### Add CSS for html tags, if requested
  if (add_html_tags) {
    res <- paste0(rock::css(template=template),
                  "\n\n",
                  res);
  }

  if (is.null(output)) {
    return(res);
  } else {
    if (dir.exists(dirname(output))) {
      if (file.exists(output) | preventOverwriting) {
        writeLines(res,
                   con = con <- file(output,
                                     "w",
                                     encoding="UTF-8"));
        close(con);
        if (!silent) {
          cat0("Wrote output file '", output,
               "' to disk.");
        }
      } else {
        if (!silent) {
          cat0("Specified output file '", output,
               "' exists, and `preventOverwriting` is set to `TRUE`; ",
               "did not write the file!");
        }
      }
      return(invisible(res));
    } else {
      stop("You passed '", output,
           "' as output filename, but directory '", dirname(output),
           "' does not exist!");
    }
  }

}
