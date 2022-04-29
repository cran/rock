#' Create a heatmap showing issues with items
#'
#' When conducting cognitive interviews, it can be useful to quickly inspect
#' the code distributions for each item. These heatmaps facilitate that
#' process.
#'
#' @param x The object with the parsed coded source(s) as resulting from a
#' call to [rock::parse_source()] or [rock::parse_sources()].
#' @param nrmSpec Optionally, an imported Narrative Response Model
#' specification, as imported with [rock::ci_import_nrm_spec()], which will
#' then be used to obtain the item labels.
#' @param language If `nrmSpec` is specified, the language to use.
#' @param itemOrder,itemLabels Instead of specifying an NRM specification,
#' you can also directly specify the item order and item labels. `itemOrder`
#' is a character vector of item identifiers, and `itemLabels` is a named
#' character vector of item labels, where each value's name is the
#' corresponding item identifier. If `itemLabels` is provided but `itemOrder`
#' is not, the order of the `itemLabel` is used.
#' @param wrapLabels Whether to wrap the labels; if not `NULL`, the
#' number of character to wrap at.
#' @param itemIdentifier The column identifying the items.
#' @param codingScheme The coding scheme, either as a string if it represents
#' one of the cognitive interviewig coding schemes provided with the `rock`
#' package, or as a coding scheme resulting from a call
#' to [rock::create_codingScheme()].
#' @param itemlab,codelab,freqlab Labels to use for the item and code axes
#' and for the frequency color legend (`NULL` to omit the label).
#' @param plotTitle The title to use for the plot
#' @param fillScale Convenient way to specify the fill scale (the colours)
#' @param theme Convenient way to specify the [ggplot2::ggplot()] theme.
#'
#' @return The heatmap as a ggplot2 plot.
#' @export
#'
#' @examples examplePath <- file.path(system.file(package="rock"), 'extdata');
#' parsedCI <- parse_source(file.path(examplePath,
#'                                    "ci_example_1.rock"));
#'
#' ci_heatmap(parsedCI,
#'            codingScheme = "peterson");
ci_heatmap <- function(x,
                       nrmSpec = NULL,
                       language = nrmSpec$defaultLanguage,
                       wrapLabels = 80,
                       itemOrder = NULL,
                       itemLabels = NULL,
                       itemIdentifier = "itemId",
                       codingScheme = "peterson",
                       itemlab = NULL,
                       codelab = NULL,
                       freqlab = "Count",
                       plotTitle = "Cognitive Interview Heatmap",
                       fillScale = ggplot2::scale_fill_viridis_c(),
                       theme = ggplot2::theme_minimal()) {

  if (!inherits(x, c("rock_parsedSource", "rock_parsedSources"))) {
    stop("As `x`, pass one or more parsed sources (as resulting from ",
         "a call to `rock::parse_source()` or `rock::parse_sources()`.");
  }

  if (is.character(codingScheme) && (length(codingScheme) == 1)) {
    codingScheme <- get0(paste0("codingScheme_", codingScheme));
  } else if (is.character(codingScheme) && (length(codingScheme) > 1)) {
    codingScheme <- create_codingScheme(
      id = "adHoc_codingScheme",
      label = "Ad Hoc Coding Scheme",
      codes = codingScheme
    );
  } else if (is.null(codingScheme)) {
    codingScheme <- create_codingScheme(
      id = "adHoc_codingScheme",
      label = "Ad Hoc Coding Scheme",
      codes = x$convenience$codingLeaves
    );
  }

  if (!inherits(codingScheme, "rock_codingScheme")) {
    stop("As `codingScheme`, pass either a codingScheme as created by ",
         "a call to `rock::create_codingScheme()`, or the name of a ",
         "coding scheme that exists in the `rock` package.");
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

  ### For convenience
  newItemCol <- tidyCodeFrequencies[, itemIdentifier];

  if (!is.null(nrmSpec)) {
    validItemLabels <-
      nrmSpec$items[[language]][
        nrmSpec$itemIds_sorted %in%
          newItemCol
      ];
  } else {
    if (is.null(itemOrder)) {
      if (is.null(itemLabels)) {
        itemOrder <- sort(unique(newItemCol));
      } else {
        itemOrder <- names(itemLabels);
      }
    }
    if (is.null(itemLabels)) {
      validItemLabels <-
        stats::setNames(itemOrder, nm = itemOrder);
    } else {
      validItemLabels <-
        itemLabels[itemOrder];
    }
  }

  if (any(!(newItemCol %in% names(validItemLabels)))) {
    validItemLabels <-
      c(validItemLabels,
        stats::setNames(
          unique(newItemCol[!(newItemCol %in% names(validItemLabels))]),
          nm = unique(newItemCol[!(newItemCol %in% names(validItemLabels))])
        )
      );
  }
  if (!is.null(wrapLabels)) {
    validItemLabels <-
      wrapVector(
        validItemLabels,
        wrapLabels
      );
  }
  newItemCol <-
    factor(
      newItemCol,
      levels = rev(names(validItemLabels)),
      labels = rev(validItemLabels),
      ordered = TRUE
    );
  tidyCodeFrequencies[, itemIdentifier] <-
    newItemCol;

  heatMap <-
    ggplot2::ggplot(data = tidyCodeFrequencies,
                    mapping = ggplot2::aes_string(
                      x = "code",
                      y = itemIdentifier,
                      fill = "frequency")
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_discrete(position = "top") +
    theme +
    fillScale +
    ggplot2::labs(x = codelab,
                  y = itemlab,
                  fill = freqlab,
                  title = plotTitle) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 1),
      plot.subtitle = ggplot2::element_text(hjust = 1),
      axis.text.x = ggplot2::element_text(angle = 30,
                                          hjust = 0)
    );

  return(heatMap);

}
