#' Generate utterance identifiers (UIDs)
#'
#' @param x The number of identifiers te generate.
#' @param origin The origin to use when generating the actual
#' identifiers. These identifiers are the present UNIX timestamp
#' (i.e. the number of seconds elapsed since the UNIX epoch,
#' the first of january 1970), accurate to two decimal places
#' (i.e. to centiseconds), converted to the base 30 system using
#' [numericToBase30()]. By default, the present time is used as
#' origin, one one centisecond is added for every identifiers to
#' generate. `origin` can be set to other values to work with
#' different origins (of course, don't use this unless you
#' understand very well what you're doing!).
#'
#' @return A vector of UIDs.
#' @export
#'
#' @examples generate_uids(5);
generate_uids <- function(x,
                          origin=Sys.time()) {

  timeNrString <- as.character(round(as.numeric(origin) * 100, 0));
  timeNrs <-
    as.numeric(timeNrString) + (0:(x-1));
  res <-
    unlist(lapply(timeNrs,
                  numericToBase30));
  return(paste0("[[uid:", res, "]]"));
}
