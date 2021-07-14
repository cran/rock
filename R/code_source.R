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
#' @param indices If `input` is a source as loaded by
#' `loading_sources`, `indices` can be used to pass a logical
#' vector of the same length as `input`
#' that indicates to which utterance the code in `codes` should be
#' applied. Note that if `indices` is provided, only the first
#' element of `codes` is used, and its name is ignored.
#' @param output If specified, the coded source will be written here.
#' @param preventOverwriting Whether to prevent overwriting existing files.
#' @param encoding The encoding to use.
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
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
#' ### Also add code "foo" to utterances with code 'ipsum'
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
                        indices = NULL,
                        output = NULL,
                        preventOverwriting = rock::opts$get('preventOverwriting'),
                        rlWarn = rock::opts$get(rlWarn),
                        encoding = rock::opts$get('encoding'),
                        silent = rock::opts$get('silent')) {

  ### Read input, if it's a file
  if ((length(input) == 1) && (file.exists(input))) {
    input <- readLines(input,
                       encoding=encoding,
                       warn = rlWarn);
    input <- cleaned_source_to_utterance_vector(input);
  } else if ("character" %in% class(input)) {
    input <- cleaned_source_to_utterance_vector(input);
  } else if (!("rock_source" %in% class(input))) {
    stop("With the `input` argument you must pass either the ",
         "path to a file with a source, a character vector ",
         "containing the source, or a ROCK source ",
         "as loaded with load_source or load_sources.\n");
  }

  codeDelimiters <- rock::opts$get(codeDelimiters);

  if (!is.null(indices) && (is.logical(indices) && (length(indices) == length(input)))) {
    ### The indices are already set as a (valid) logical vector
    codeToAdd <- paste0(codeDelimiters[1],
                        codes[1],
                        codeDelimiters[2]);
    if (!silent) {
      cat0("The 'indices' argument is a logical vector indicating to which utterances to apply code '",
                codes[1], "' (specifically, the utterances on lines ",
                vecTxt(which(indices)), ").\n");
    }
  } else if (!is.null(indices) && (is.numeric(indices)) && ((min(indices) >= 1) && (max(indices) <= length(input)))) {
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
      if (length(codes) > 1) {
        cat0("Multiple codes to check have been specified.");
      }
      cat0("Starting processing of ",
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
        if (as.numeric(names(codes)[i]) <= length(input)) {
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

  if (is.null(output)) {
    class(input) <- c("rock_source", "character");
    return(input);
  } else {

    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    if (file.exists(output) && preventOverwriting) {
      if (!silent) {
        message("File '",
                output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                "write the source with added codes to disk.");
      }
    } else {
      con <- file(description=output,
                  open="w",
                  encoding=encoding);
      writeLines(text=input,
                 con=con);
      close(con);
    }
    if (!silent) {
      message("I just wrote a source with added codes to file '",
              output,
              "'.");
    }

    class(input) <- c("rock_source", "character");
    return(invisible(input));

  }


}
