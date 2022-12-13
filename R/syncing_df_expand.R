#' @rdname expanding_vectors_or_dataframes
#' @export
syncing_df_expand <- function(x,
                              newLength,
                              fill = TRUE,
                              expandFun = NULL,
                              silent = rock::opts$get('silent')) {

  res <-
    lapply(
      x,
      syncing_vector_expand,
      newLength = newLength,
      fill = fill,
      silent = silent
    );

  res <- as.data.frame(res);

  names(res) <- names(x);

  return(res);

}
