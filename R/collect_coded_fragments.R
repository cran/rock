#' Create an overview of coded fragments
#'
#' Collect all coded utterances and optionally add some context
#' (utterances before and utterances after) to create ann overview
#' of all coded fragments per code.
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
#' @param rawResult Whether to return the raw result, a list of the
#' fragments, or one character value in markdown format.
#' @param output Here, a path and filename can be provided where the
#' result will be written. If provided, the result will be returned
#' invisibly.
#' @param cleanUtterances Whether to use the clean or the raw utterances
#' when constructing the fragments (the raw versions contain all codes).
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
                                    rawResult = FALSE,
                                    output = NULL,
                                    cleanUtterances = TRUE,
                                    silent=TRUE) {

  if (!("rockParsedSource" %in% class(x)) &&
      !("rockParsedSources" %in% class(x))) {
    stop(glue::glue("The object you provided (as argument `x`) has class '{vecTxtQ(class(x))}', ",
                    "but I can only process objects obtained by parsing one or more sources (with ",
                    "`rock::parse_source` or `rock::parse_sources`), which have class 'rockParsedSource' ",
                    "or 'rockParsedSources'."));
  }

  codes <- grep(codes,
                x$convenience$codingLeaves,
                value=TRUE);
  dat <- x$mergedSourceDf;

  if (!silent) {
    cat0("The regular expression passed in argument `codes` ('",
              codes, "') matches the following codings: ",
              vecTxtQ(codes), ".\n\n");
  }

  ### Get line numbers of the fragments to extract,
  ### get fragments, store them
  res <- lapply(codes,
                function(i) {
                  return(lapply(which(dat[, i] == 1),
                           function(center) {
                             res <- seq(center - context,
                                        center + context);
                             ### Shift forwards or backwards to make sure early or late
                             ### fragments don't exceed valid utterance (line) numbers
                             res <- res - min(0, (min(res) - 1));
                             res <- res - max(0, (max(res) - nrow(dat)));
                             if (cleanUtterances) {
                               return(paste0(dat[res, 'utterances_clean'],
                                             collapse="\n"));
                             } else {
                               return(paste0(dat[res, 'utterances_raw'],
                                             collapse="\n"));
                             }
                           }));
                });

  if (rawResult) {
    names(res) <-
      codes;
  } else {
    ### Set codePrefix based on whether a heading
    ### will be included
    if (is.null(heading)) {
      if (length(codes) > 5) {
        heading <-
          paste0(repStr("#", headingLevel), " ",
                 "Collected coded fragments with ",
                 context, " lines of context",
                 "\n\n");
      } else {
        heading <-
          paste0(repStr("#", headingLevel), " ",
                 "Collected coded fragments for codes ",
                 vecTxtQ(codes), " with ",
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
                  collapse="\n\n-----\n\n");
    ### Unlist into vector
    res <- unlist(res);
    ### Add titles
    res <- paste0(codePrefix, codes, "\n\n-----\n\n",
                  res, "\n\n-----\n");
    ### Collapse into one character value
    res <- paste0(res, collapse="\n");
    ### Add title heading
    if (!identical(heading, FALSE)) {
      res <- paste0(heading,
                    res);
    }
  }

  if (is.null(output)) {
    return(res);
  } else {
    if (dir.exists(dirname(output))) {
      writeLines(res,
                 con = con <- file(output,
                                   "w",
                                   encoding="UTF-8"));
      close(con);
      return(invisible(res));
    } else {
      stop("You passed '", output,
           "' as output filename, but directory '", dirname(output),
           "' does not exist!");
    }
  }

}
