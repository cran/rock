#' Add one or more codes to one or more sources
#'
#' These functions add codes to one or more
#' sources that were read with one of the
#' `loading_sources` functions.
#'
#' @param input The source, or list of sources, as
#' produced by one of the `loading_sources` functions.
#' @param codes A named character vector, where each element
#' is the code to be added to the matching utterance, and
#' the corresponding name is either an utterance identifier
#' (in which case the utterance with that identifier will be
#' coded with that code), a code (in which case all utterances
#' with that code will be coded with the new code as well), a
#' digit (in which case the utterance at that line number in
#' the source will be coded with that
#' code), or a regular expression, in which case all utterances
#' matching that regular expression will be coded with that
#' source. If specifying an utterance ID or code, make sure
#' that the code delimiters are included (normally, two square
#' brackets).
#' @param indices A logical vector of the same length as `input`
#' that indicates to which utterance the code in `codes` should be
#' applied. Note that if `indiced` is provided, only the first
#' element of `codes` is used, and its name is ignored.
#' @param codeDelimiters A character vector of two elements
#' specifying the opening and closing delimiters of codes (conform
#' the default ROCK convention, two square brackets). The square
#' brackets will be escaped; other characters will not, but will
#' be used as-is.
#' @param silent Whether to be chatty or quiet.
#'
#' @return Invisibly, the coded source object.
#' @rdname coding_sources
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' loadedExample <- rock::load_source(exampleFile);
#'
#' ### Show line 71
#' cat(loadedExample[71]);
#'
#' ### Specify the rules to code all utterances
#' ### containing "Ipsum" with the code 'ipsum' and
#' ### all utterances containing the code
#' codeSpecs <-
#'   c("(?i)ipsum" = "ipsum",
#'     "BC|AD|\\d\\d\\d\\ds" = "timeRef");
#'
#' ### Apply rules
#' codedExample <- code_source(loadedExample,
#'                             codeSpecs);
#'
#' ### Show line 71
#' cat(codedExample[71]);
#'
#' ### Also add code "foo" to utteranced with code 'ipsum'
#' moreCodedExample <- code_source(codedExample,
#'                                 c("[[ipsum]]" = "foo"));
#'
#' ### Show line 71
#' cat(moreCodedExample[71]);
#'
#' ### Use the 'indices' argument to add the code 'bar' to
#' ### line 71
#' overCodedExample <- code_source(moreCodedExample,
#'                                 "bar",
#'                                 indices=71);
#'
#' cat(overCodedExample[71]);
#'
#' @export
code_source <- function(input,
                        codes,
                        indices=NULL,
                        codeDelimiters = c("[[", "]]"),
                        silent=TRUE) {

  if (!("rock_source" %in% class(input))) {
    stop("With the `input` argument you must pass a ROCK source ",
         "as loaded with load_source or load_sources.\n");
  }

  if (!is.null(indices) && (is.logical(indices) && (length(indices) == length(input)))) {
    ### The indices are already set as a (valid) logical vector
    codeToAdd <- paste0(codeDelimiters[1],
                        codes[1],
                        codeDelimiters[2]);
    if (!silent) {
      cat0("The first argument is a logical vector indicating to which utterances to apply code '",
                codes[1], "' (specifically, the utterances on lines ",
                vecTxt(which(indices)), ").\n");
    }
  } else if (!is.null(indices) && (is.numeric(indices)) && ((min(indices) > 1) && (max(indices) <= length(input)))) {
    ### The indices are already set as a (valid) numeric vector
    codeToAdd <- paste0(codeDelimiters[1],
                        codes[1],
                        codeDelimiters[2]);
    if (!silent) {
      cat0("The first argument is a numeric vector indicating to which utterances to apply code '",
                codes[1], "' (specifically, the utterances on lines ",
                vecTxt(indices), ").\n");
    }
  } else {

    ### Create regex to match codes, where we escape the character
    ### class specification codes (square brackets)
    regexMatchingCode <-
      paste0("^",
             escapeRegexCharacterClass(codeDelimiters[1]),
             "(.*)",
             escapeRegexCharacterClass(codeDelimiters[2]),
             "$");

    if (!silent) {
      cat0("Multiple codes to check have been specified. Starting processing of ",
                vecTxtQ(names(codes)),
                " against the regular expression ",
                vecTxtQ(regexMatchingCode), ".\n");
    }

    for (i in seq_along(codes)) {

      ### Generate code to add to utterances matching this code
      codeToAdd <-
        paste0(codeDelimiters[1],
               codes[i],
               codeDelimiters[2]);

      if (any(grepl(regexMatchingCode,
                    names(codes)[i],
                    perl=TRUE))) {

        ### The name of this case is a code or an utterance ID;
        ### Extract it from between the specified delimiters
        escapedCodeToFind <-
          escapeRegexCharacterClass(names(codes)[i]);

        ### Get indices matching it
        indices <-
          grep(escapedCodeToFind,
               input,
               perl=TRUE);

        if (!silent) {
          cat0("Looking for code or utterance id '",
                    escapedCodeToFind, "'; found in utterances with line numbers ",
                    vecTxt(indices), ".\n");
        }

      } else if (grepl("^[0-9]+$", names(codes)[i])) {
        ### It's a number, so a line number; check whether
        ### it's not too high and then add code
        if (names(codes)[i] <= length(input)) {
          ### (Only one index, really)
          indices <- as.numeric(names(codes)[i]);
        } else {
          stop("You specified a line number (",
               names(codes)[i], ") that's higher than ",
               "the number of lines in the source (",
               length(input),
               ")!\n");
        }
      } else {
        ### The name of this case is a a regular expression;
        ### get indices matching it
        indices <- grep(names(codes)[i],
                        input,
                        perl=TRUE);
        if (!silent) {
          cat0("Looking for a text match with regular expression '",
                    names(codes)[i], "'; found in utterances with line numbers ",
                    vecTxt(indices), ".\n");
        }
      }

      ### Append code
      input[indices] <-
        paste(input[indices],
              codeToAdd,
              sep=" ");

      if (!silent) {
        cat0("Appending code '", codeToAdd, "' to utterances at those line numbers.\n");
      }
    }
  }

  res <- input;

  class(res) <- c("rock_source", "character");

  return(invisible(res));

}
