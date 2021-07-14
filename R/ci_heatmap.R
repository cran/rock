#' Create a heatmap showing issues with items
#'
#' When conducting cognitive interviews, it can be useful to quickly inspect
#' the code distributions for each item. These heatmaps facilitate that
#' process.
#'
#' @param x The object with the parsed coded source(s) as resulting from a
#' call to [rock::parse_source()] or [rock::parse_sources()].
#' @param itemIdentifier The column identifying the items.
#' @param codingScheme The coding scheme, either as a string if it represents
#' one of the cognitive interviewig coding schemes provided with the `rock`
#' package, or as a coding scheme resulting from a call
#' to [rock::create_codingScheme()].
#' @param itemlab,codelab,freqlab Labels to use for the item and code axes
#' and for the frequency color legend.
#' @param plotTitle The title to use for the plot
#' @param fillScale Convenient way to specify the fill scale (the colours)
#' @param theme Convenient way to specify the [ggplot2::ggplot()] theme.
#'
#' @return The heatmap
#' @export
#'
#' @examples examplePath <- file.path(system.file(package="rock"), 'extdata');
#' parsedCI <- parse_source(file.path(examplePath,
#'                                    "ci_example_1.rock"));
#'
#' ci_heatmap(parsedCI,
#'            codingScheme = "peterson");
ci_heatmap <- function(x,
                       itemIdentifier = "uiid",
                       codingScheme = "peterson",
                       itemlab = "Item",
                       codelab = "Code",
                       freqlab = "Frequency",
                       plotTitle = "Cognitive Interview Heatmap",
                       fillScale = ggplot2::scale_fill_viridis_c(),
                       theme = ggplot2::theme_minimal()) {

  if (is.character(codingScheme) && (length(codingScheme) == 1)) {
    codingScheme <- get0(paste0("codingScheme_", codingScheme));
  } else if (is.character(codingScheme) && (length(codingScheme) > 1)) {
    codingScheme <- create_codingScheme(
      id = "adHoc_codingScheme",
      label = "Ad Hoc Coding Scheme",
      codes = codingScheme
    );
  }

  if (!inherits(codingScheme, "rock_codingScheme")) {
    stop("As `codingScheme`, pass either a codingScheme as created by ",
         "a call to `rock::create_codingScheme()`, or the name of a ",
         "coding scheme that exists in the `rock` package.");
  }

  if (!inherits(x, c("rock_parsedSource", "rock_parsedSources"))) {
    stop("As `x`, pass one or more parsed sources (as resulting from ",
         "a call to `rock::parse_source()` or `rock::parse_sources()`.");
  }

  mergedSourceDf <- x$mergedSourceDf;

  usedCodes <- intersect(
    codingScheme$codes,
    names(mergedSourceDf)
  );

  if (length(usedCodes) == 0) {
    stop("None of the codes in coding scheme ", codingScheme$label,
         " was used for the coded source(s)!");
    return(invisible(NULL));
  }

  codeFrequencyTable <-
    do.call(
      rbind,
      by(
        data = mergedSourceDf[, usedCodes],
        INDICES = mergedSourceDf[, itemIdentifier],
        FUN = colSums
      )
    );

  tidyCodeFrequencies <-
    data.frame(
      rep(rownames(codeFrequencyTable), ncol(codeFrequencyTable)),
      rep(colnames(codeFrequencyTable), each=nrow(codeFrequencyTable)),
      as.vector(codeFrequencyTable)
    );
  names(tidyCodeFrequencies) <- c(itemIdentifier, "code", "frequency");

  heatMap <-
    ggplot2::ggplot(data = tidyCodeFrequencies,
                    mapping = ggplot2::aes_string(
                      x = "code",
                      y = itemIdentifier,
                      fill = "frequency")
    ) +
    ggplot2::geom_tile() +
    theme +
    fillScale +
    ggplot2::labs(x = codelab,
                  y = itemlab,
                  fill = freqlab,
                  title = plotTitle);

  return(heatMap);

}
