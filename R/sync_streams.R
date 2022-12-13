#' Synchronize multiple streams
#'
#' This function maps the codes from multiple streams onto a primary stream.
#'
#' @param x The object with the parsed sources.
#' @param primaryStream The identifier of the primary stream.
#' @param columns The names of the column(s) to synchronize.
#' @param anchorsCol The column containing the anchors.
#' @param sourceId The column containing the source identifiers.
#' @param streamId The column containing the stream identifiers.
#' @param prependStreamIdToColName,appendStreamIdToColName Whether to append
#' or prepend the stream identifier before merging the dataframes together.
#' @param colNameGlue When appending or prepending stream identifiers, the
#' character(s) to use as "glue" or separator.
#' @param silent Whether to be silent (`TRUE`) or chatty (`FALSE`).
#'
#' @return The object with parsd sources, `x`, with the synchronization results
#' added in the `$syncResults` subobject.
#' @inheritParams syncing_vector_compress
#' @inheritParams syncing_vector_expand
#' @export
#'
#' @examples ### Get a directory with example sources
#' examplePath <-
#'   file.path(
#'     system.file(package="rock"),
#'     'extdata',
#'     'streams'
#'   );
#'
#' ### Parse the sources
#' parsedSources <- rock::parse_sources(
#'   examplePath
#' );
#'
#' ### Add a dataframe, syncing all streams to primary stream !
#' parsedSources <- rock::sync_streams(
#'   parsedSources,
#'   primaryStream = "streamA",
#'   columns = c("Code1", "Code2", "Code3"),
#'   prependStreamIdToColName = TRUE
#' );
#'
#' ### Look at two examples
#' parsedSources$syncResults$mergedSourceDf[
#'   ,
#'   c("streamB_Code3", "streamC_Code1")
#' ];
sync_streams <- function(x,
                         primaryStream,
                         columns = NULL,
                         anchorsCol = rock::opts$get('anchorsCol'),
                         sourceId = rock::opts$get('sourceId'),
                         streamId = rock::opts$get('streamId'),
                         prependStreamIdToColName = FALSE,
                         appendStreamIdToColName = FALSE,
                         sep = " ",
                         fill = TRUE,
                         compressFun = NULL,
                         compressFunPart = NULL,
                         expandFun = NULL,
                         colNameGlue = rock::opts$get('colNameGlue'),
                         silent = rock::opts$get('silent')) {

  df <- x$mergedSourceDf;

  if (is.null(columns)) {
    columns <- names(df);
  }

  sourceIds <-
    unique(df[, sourceId]);

  sourceIds <-
    sourceIds[!is.na(sourceIds)];

  streamIds <-
    unique(df[, streamId]);

  streamIds <-
    streamIds[!is.na(streamIds)];

  dfBySource <-
    lapply(
      sourceIds,
      function(currentSourceId) {
        return(
          df[df[, sourceId] == currentSourceId, ]
        );
      }
    );

  names(dfBySource) <- sourceIds;

  dfBySourceAndStream <-
    lapply(
      dfBySource,
      function(currentDfBySource) {
        res <-
          lapply(
            streamIds,
            function(currentStreamId) {
              ### Extract relevant data frame rows
              return(
                currentDfBySource[
                  currentDfBySource[, streamId] == currentStreamId, ]
              );
            }
          );
        names(res) <-
          streamIds;
        return(res);
      }
    );

  ### Now we have the dataframes neatly split by source and stream,
  ### get the vectors with anchors
  fullAnchorVectors <-
    lapply(
      dfBySourceAndStream,
      function(currentDfBySource) {
        return(
          lapply(
            currentDfBySource,
            function(currentDfBySourceAndStream) {
              return(currentDfBySourceAndStream[, anchorsCol]);
            }
          )
        );
      }
    );

  anchorOnlyVectors <-
    lapply(
      fullAnchorVectors,
      function(currentSourceAnchors) {
        return(
          lapply(
            currentSourceAnchors,
            function(allAnchors) {
              ### Get and check anchors
              anchorsOnly <-
                sort(
                  allAnchors[!is.na(allAnchors) & nchar(allAnchors) > 0]
                );
              if (any(duplicated(anchorsOnly))) {
                stop("Found a duplicate anchor!");
              }
              return(anchorsOnly);
            }
          )
        );
      }
    );

  ###---------------------------------------------------------------------------
  ### Check for uniqueness
  ###
  ### Note: if we ever want to make it possible to have fewer anchors in the
  ### non-primary streams, this bit below has to be updated.
  ###---------------------------------------------------------------------------
  lapply(
    anchorOnlyVectors,
    function(anchorVectorForCurrentSource) {
      anchorDf <-
        as.data.frame(
          anchorVectorForCurrentSource
        );
      res <-
        lapply(
          as.data.frame(
            t(
              anchorDf
            )
          ),
          unique
        );
      if (!all(unlist(lapply(res, length)) == 1)) {
        stop(
          "Not all anchors align!\n\n",
          paste0(
            utils::capture.output(
              print(anchorDf)
            ),
            collapse="\n"
          ),
          "\n\n"
        );
      }
      return(anchorDf);
    }
  );

  ###---------------------------------------------------------------------------
  ### Get anchor locations and pairs from every stream
  ###---------------------------------------------------------------------------

  anchorIndices <- list();

  for (currentSourceName in names(anchorOnlyVectors)) {

    currentSource <- anchorOnlyVectors[[currentSourceName]];
    anchorIndices[[currentSourceName]] <- list();

    for (currentStream in names(currentSource)) {

      anchorIndices[[currentSourceName]][[currentStream]] <- list();

      for (currentStartAnchor in 1:(length(currentSource[[primaryStream]])-1)) {

        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]] <-
          list(
            startAnchor = currentSource[[primaryStream]][currentStartAnchor],
            endAnchor = currentSource[[primaryStream]][currentStartAnchor + 1]
          );
        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex <-
          which(
            fullAnchorVectors[[currentSourceName]][[currentStream]] ==
              anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startAnchor
          );
        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endIndex <-
          which(
            fullAnchorVectors[[currentSourceName]][[currentStream]] ==
              anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endAnchor
          );

        if (anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endIndex - 1 ==
            anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex) {
          anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract <-
            c();
        } else {
          anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract <-
            (anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$startIndex+1):
            (anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$endIndex-1);
        }

        anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$length <-
          length(
            anchorIndices[[currentSourceName]][[currentStream]][[currentStartAnchor]]$indicesToExtract
          );

      }

    }
  }

  ###---------------------------------------------------------------------------
  ### Create vectors with new indices for each non-primary stream
  ###---------------------------------------------------------------------------

  msg("Starting stream synchronization. Primary stream: ", primaryStream, ".\n",
      silent = silent);

  streamMapping <-
    lapply(
      names(anchorIndices),
      function(currentSourceName) {
        currentSource <- anchorIndices[[currentSourceName]];
        otherStreams <- setdiff(names(currentSource), primaryStream);

        msg(" - Processing source with identifier: ", currentSourceName, ".\n",
            silent = silent);

        res <-
          lapply(
            names(currentSource[otherStreams]),
            function(currentStreamName) {

              currentStream <- currentSource[[currentStreamName]];
              res <- list();

              msg("  - Processing stream with identifier: ", currentStreamName, ".\n",
                  silent = silent);

              for (anchorPair in seq_along(currentStream)) {

                msg("   - Processing anchor pair ",
                    currentStream[[anchorPair]]$startAnchor,
                    ":", currentStream[[anchorPair]]$endAnchor,
                    " at indices ",
                    currentStream[[anchorPair]]$startIndex, ":",
                    currentStream[[anchorPair]]$endIndex, ", so ",
                    currentStream[[anchorPair]]$length,
                    " indices to sync to length ",
                    currentSource[[primaryStream]][[anchorPair]]$length,
                    ".\n",
                    silent = silent);

                if (currentStream[[anchorPair]]$length == 0) {

                  msg("     No utterances in this anchor pair (their indices ",
                      "are ", currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      "). Returning empty data frame of ",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      " rows.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    create_glue_df(
                      dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                        ,
                        columns,
                        drop=FALSE
                      ],
                      rows = currentSource[[primaryStream]][[anchorPair]]$length
                    );

                } else if (currentSource[[primaryStream]][[anchorPair]]$length ==
                           currentStream[[anchorPair]]$length) {

                  msg("     This anchor pair (with indices ",
                      currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      ") is already the same size as the primary stream (",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      "), returning as is.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                      currentStream[[anchorPair]]$indicesToExtract,
                      columns,
                      drop = FALSE
                    ];

                } else if (currentSource[[primaryStream]][[anchorPair]]$length <
                           currentStream[[anchorPair]]$length) {

                  msg("     This anchor pair (with indices ",
                      currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      ") is longer than the primary stream (",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      "), compressing to that size.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    syncing_df_compress(
                      dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                        currentStream[[anchorPair]]$indicesToExtract,
                        columns,
                        drop=FALSE
                      ],
                      newLength = currentSource[[primaryStream]][[anchorPair]]$length,
                      sep = sep,
                      compressFun = compressFun,
                      compressFunPart = compressFunPart,
                      silent = silent
                    );

                } else if (currentSource[[primaryStream]][[anchorPair]]$length >
                           currentStream[[anchorPair]]$length) {

                  msg("     This anchor pair (with indices ",
                      currentStream[[anchorPair]]$startIndex,
                      " and ", currentStream[[anchorPair]]$endIndex,
                      ") is shorter than the primary stream (",
                      currentSource[[primaryStream]][[anchorPair]]$length,
                      "), expanding to that size.\n",
                      silent = silent);

                  res[[anchorPair]] <-
                    syncing_df_expand(
                      dfBySourceAndStream[[currentSourceName]][[currentStreamName]][
                        currentStream[[anchorPair]]$indicesToExtract,
                        columns,
                        drop=FALSE
                      ],
                      newLength = currentSource[[primaryStream]][[anchorPair]]$length,
                      fill = fill,
                      expandFun = expandFun
                    );

                }
              }

              return(res);

            }
          );
        names(res) <- names(currentSource[otherStreams]);
        return(res);
      }
    );

  names(streamMapping) <- names(anchorIndices);

  ###---------------------------------------------------------------------------
  ### Merge these dataframes into one dataframe per stream per source
  ###---------------------------------------------------------------------------

  mergedStreamDfs <-
    lapply(
      streamMapping,
      function(currentSource) {
        res <-
          lapply(
            names(currentSource),
            function(currentStreamName) {
              res <-
                glue_df_list(
                  currentSource[[currentStreamName]]
                );
              if (prependStreamIdToColName) {
                names(res) <- paste0(currentStreamName,
                                     colNameGlue,
                                     names(res));
              }
              if (appendStreamIdToColName) {
                names(res) <- paste0(names(res),
                                     colNameGlue,
                                     currentStreamName);
              }
              return(res);
            }
          );
        names(res) <- names(currentSource);
        return(res);
      }
    );

  ###---------------------------------------------------------------------------
  ### Merge dataframes for each source
  ###---------------------------------------------------------------------------

  syncedStreamDfs <-
    lapply(
      names(dfBySourceAndStream),
      function(sourceDfName) {
        return(
          do.call(
            cbind,
            c(
              list(
                dfBySourceAndStream[[
                  sourceDfName
                ]][[
                  primaryStream
                ]]
              ),
              unname(
                mergedStreamDfs[[
                  sourceDfName
                ]]
              )
            )
          )
        );
      }
    );

  names(syncedStreamDfs) <- names(dfBySourceAndStream);

  ###---------------------------------------------------------------------------
  ### Merge the synced stream data framesinto one new 'mergedSourceDf'
  ###---------------------------------------------------------------------------

  mergedSourceDf <-
    rbind_df_list(
      syncedStreamDfs
    );

  ###---------------------------------------------------------------------------
  ### Compose sub-object with results and return it
  ###---------------------------------------------------------------------------

  x$syncResults <-
    list(
      sourceIds = sourceIds,
      streamIds = streamIds,
      dfBySource = dfBySource,
      dfBySourceAndStream = dfBySourceAndStream,
      fullAnchorVectors = fullAnchorVectors,
      anchorOnlyVectors = anchorOnlyVectors,
      anchorIndices = anchorIndices,
      streamMapping = streamMapping,
      mergedStreamDfs = mergedStreamDfs,
      syncedStreamDfs = syncedStreamDfs,
      mergedSourceDf = mergedSourceDf
    );

  return(
    invisible(
      x
    )
  );

}
