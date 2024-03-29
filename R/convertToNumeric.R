#' Conveniently convert vectors to numeric
#'
#' Tries to 'smartly' convert factor and character vectors to numeric.
#'
#' @param vector The vector to convert.
#' @param byFactorLabel When converting factors, whether to do this
#' by their label value (`TRUE`) or their level value (`FALSE`).
#'
#' @return The converted vector.
#' @export
#'
#' @examples rock::convertToNumeric(as.character(1:8));
convertToNumeric <- function(vector, byFactorLabel = FALSE) {
  ### Check whether the vector is datetime
  if (sum(sapply(class(vector), grepl, pattern='POSIX')) > 0) {
    return(vector);
  }
  if (!(is.factor(vector) | is.numeric(vector) |
        is.character(vector) | is.logical(vector))) {
    stop("Argument 'vector' must be a vector! Current class = '",
         class(vector), "'. To mass convert e.g. a dataframe, ",
         "use massConvertToNumber.");
  }
  if(is.factor(vector) && byFactorLabel) {
    ### Decimal symbol might be a comma instead of a period: convert
    ### factor to character vector and replace commas with periods
    vector <- as.numeric(gsub(as.character(vector), pattern=",",
                              replacement="."),
                         fixed=TRUE);
    return();
  }
  else if (is.character(vector)) {
    return(suppressWarnings(as.numeric(gsub(as.character(vector),
                                            pattern=",", replacement="."))));
  }
  else {
    ### Thus, for numeric vectors; factors to be converted by index of the levels
    ### instead of by their labels; and logical vectors.
    return(as.numeric(as.character(vector)));
  }
}
