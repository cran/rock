#' Get the code identifiers of the children of a code with a given identifier
#'
#' @param x The parsed sources object
#' @param parentCodeId The code identifier of the parent code
#' @param returnNodes Set to `TRUE` to return a list of nodes, not just the
#' code identifiers
#'
#' @return A character vector with code identifiers (or a list of nodes)
#' @export
get_childCodeIds <- function(x,
                             parentCodeId,
                             returnNodes = FALSE) {

  if ((!inherits(x, "rock_parsedSources")) &&
      (!inherits(x, "rock_parsedSource"))) { ### Added this, might be wrong
    stop("As `x`, you have to pass a parsed sources object. You passed ",
            deparse(substitute(x)), "', which instead has class(es) ",
            vecTxtQ(class(x)), ".");
  }

  if ((!is.character(parentCodeId)) | (length(parentCodeId) != 1)) {
    stop("As `parentCodeId`, you have to pass a single character value. ",
         "However, what you passed is either not a character value, or ",
         "has a length other than 1 (i.e. 0, 2, or larger).");
  }

  if (inherits(x, "rock_parsedSource")) {
    node <- NULL;
    for (i in seq_along(x$inductiveCodeTrees)) {
      if (!is.null(x$inductiveCodeTrees[[i]])) {
        node <-
          data.tree::FindNode(x$inductiveCodeTrees[[i]], parentCodeId);
      }
    }
  } else {
    node <-
      data.tree::FindNode(x$fullyMergedCodeTrees, parentCodeId);
  }

  if (is.null(node)) {
    stop("In the parsed sources object that you passed (",
         substitute(deparse(x)), "), no code identifier '",
         parentCodeId, "' was found.");
  }

  childNodes <- node$children;

  if (returnNodes) {
    return(childNodes);
  } else {
    if (length(childNodes) == 0) {
      return(NA);
    } else {
      return(names(childNodes));
    }
  }

}
