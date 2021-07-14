#' Match the corresponding indices of (YAML) delimiters in a sequantial list
#'
#' @param x The vector with delimiter indices
#' @param errorOnInvalidX Whether to return `NA` (if `FALSE`) or throw an
#' error (if `TRUE`) when `x` is `NULL`, `NA`, or has less than 2 elements.
#' @param errorOnOdd Whether to throw an error if the number of delimiter
#' indices is odd.
#' @param onOddIgnoreFirst If the number of delimiter indices is odd and no
#' error is thrown, whether to ignore the first (`TRUE`) or the last (`FALSE`)
#' delimiter.
#'
#' @export
#'
match_consecutive_delimiters <- function(x,
                                         errorOnInvalidX = FALSE,
                                         errorOnOdd = FALSE,
                                         onOddIgnoreFirst = FALSE) {
  if (length(x) < 2) {
    if (errorOnInvalidX) {
      stop("The vector with delimiters you passed cannot be matched.");
    } else {
      return(NA);
    }
  }
  ### Check whether we have an odd number of delimiters
  if ((length(x) %% 2) != 0) {
    if (errorOnOdd) {
      stop("An uneven number of delimiters (", length(x), ") was passed!");
    } else if (onOddIgnoreFirst) {
      x <- utils::tail(x, -1);
    } else {
      x <- utils::head(x, -1);
    }
  }
  xIndices <- seq_along(x);
  startIndices <- ((xIndices %% 2) != 0);
  endIndices <- ((xIndices %% 2) == 0);
  return(
    mapply(
      c,
      x[startIndices],
      x[endIndices],
      SIMPLIFY = FALSE
    )
  );
}
