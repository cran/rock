#' Merge source files by different coders
#'
#' This function takes sets of sources and merges them using the utterance
#' identifiers (UIDs) to match them.
#'
#' @inheritParams parse_sources
#' @param input The directory containing the input sources.
#' @param output The path to the directory where to store the merged sources.
#' This path will be created with a warning if it does not exist. An exception
#' is if "`same`" is specified - in that case, every file will be written to the
#' same directory it was read from.
#' @param outputPrefix,outputSuffix A pre- and/or suffix to add to the filename
#' when writing the merged sources (especially useful when writing them to the
#' same directory).
#' @param primarySourcesRegex A regular expression that specifies how to
#' recognize the primary sources (i.e. the files used as the basis, to which
#' the codes from other sources are added).
#' @param primarySourcesIgnoreRegex A regular expression that specifies which
#' files to ignore as primary files.
#' @param primarySourcesPath The path containing the primary sources.
#' @param recursive,primarySourcesRecursive Whether to read files from
#' sub-directories (`TRUE`) or not.
#' @param filenameRegex Only files matching this regular expression are read.
#' @param preventOverwriting Whether to prevent overwriting existing files or not.
#' @param inheritSilence If not silent, whether to let functions called
#' by `merge_sources` inherit that setting.
#'
#' @return Invisibly, a list of the parsed, primary, and merged sources.
#' @export
merge_sources <- function(input,
                          output,
                          outputPrefix = "",
                          outputSuffix = "_merged",
                          primarySourcesRegex=".*",
                          primarySourcesIgnoreRegex=outputSuffix,
                          primarySourcesPath = input,
                          recursive = TRUE,
                          primarySourcesRecursive = recursive,
                          filenameRegex = ".*",
                          postponeDeductiveTreeBuilding = TRUE,
                          ignoreOddDelimiters=FALSE,
                          preventOverwriting = rock::opts$get(preventOverwriting),
                          encoding=rock::opts$get(encoding),
                          silent=rock::opts$get(silent),
                          inheritSilence = FALSE) {

  if (!dir.exists(primarySourcesPath)) {
    stop("Directory specified to read primary sources from (",
         primarySourcesPath, ") does not exist!");
  }

  ### Store all arguments and delete the ones specific to this function
  # args <- as.list(environment());
  # args <-
  #   args[setdiff(names(args),
  #                c('primarySourcesRegex',
  #                  'primarySourcesIgnoreRegex',
  #                  'primarySourcesPath',
  #                  'primarySourcesRecursive',
  #                  'output',
  #                  'outputPrefix',
  #                  'outputSuffix',
  #                  'preventOverwriting',
  #                  'inheritSilence'))];
  # ### Set 'silent' as function of both imperative for this function and the called functions
  # args$silent <- ifelse(inheritSilence, silent, TRUE);

  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);
  autoGenerateIds <- rock::opts$get(autoGenerateIds);
  persistentIds <- rock::opts$get(persistentIds);
  utteranceMarker <- rock::opts$get(utteranceMarker);
  noCodes <- rock::opts$get(noCodes);
  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);
  attributeContainers <- rock::opts$get(attributeContainers);
  codesContainers <- rock::opts$get(codesContainers);
  delimiterRegEx <- rock::opts$get(delimiterRegEx);
  ignoreRegex <- rock::opts$get(ignoreRegex);
  coderId <- rock::opts$get(coderId);
  idForOmittedCoderIds <- rock::opts$get(idForOmittedCoderIds);

  if (!silent) {
    cat0("\n\nStarting to extract all codings from all sources in directory '",
         input, "' that match regular expression '", filenameRegex, "'.");
  }

  ### Then pass arguments along to extract_codings_by_coderId and store result
  parsedSources <-
    do.call(extract_codings_by_coderId,
            list(input=input,
                 recursive = recursive,
                 filenameRegex = filenameRegex,
                 ignoreOddDelimiters=ignoreOddDelimiters,
                 postponeDeductiveTreeBuilding = postponeDeductiveTreeBuilding,
                 encoding=encoding,
                 silent=ifelse(inheritSilence, silent, TRUE)));

  allCodedUtterances <-
    names(parsedSources$utterances);

  if (!silent) {
    cat0("\n\nProcessed codings for a total of ",
         length(allCodedUtterances), " utterances, where the first six utterance identifiers are ",
         vecTxtQ(utils::head(allCodedUtterances)), " and the last six utterance identifiers are ",
         vecTxtQ(utils::tail(allCodedUtterances)), ".\n\n");
  }

  if (!silent) {
    cat0("\n\nStarting to load primary sources in directory '",
         primarySourcesPath, "'.\n\n");
  }

  ### Read primary sources
  primarySources <-
    load_sources(input = primarySourcesPath,
                 encoding=encoding,
                 filenameRegex=primarySourcesRegex,
                 recursive=primarySourcesRecursive,
                 full.names=TRUE,
                 ignoreRegex=primarySourcesIgnoreRegex,
                 silent=ifelse(inheritSilence, silent, TRUE));

  if (!silent) {
    cat0("\n\nLoaded ", length(primarySources), " primary sources.");
  }

  if (!(tolower(output) == "same")) {
    if (!dir.exists(output)) {
      warning("Directory provided to write to ('",
              output,
              "') does not exist - creating it!");
      dir.create(output,
                 recursive = TRUE);
    }
  }

  if (!silent) {
    cat0("\n\nStarting to merge codes into primary sources.\n");
  }

  mergedSources <- list();
  primarySourceUids <- list();
  for (i in names(primarySources)) {

    if (!silent) {
      cat0("\n - Starting to process primary source '", basename(i), "'.");
    }

    ### Check for matches with UID regex
    uidRegexMatches <-
      grepl(uidRegex,
            primarySources[[i]],
            perl=TRUE);

    if (!silent) {
      cat0("\n   - Out of the ", length(uidRegexMatches), " lines in this source, ",
           sum(uidRegexMatches), " match the uidRegex (i.e. contain an utterance identifier).");
    }

    ### Extract the UIDs and store in a vector, where every element corresponds to a line in the source.
    primarySourceUids[[i]] <-
      ifelse(uidRegexMatches,
             gsub(paste0(".*", uidRegex, ".*"),
                  "\\1",
                  primarySources[[i]]),
             "");

    ### Store primary source as basis for the merged source text
    mergedSources[[i]] <- primarySources[[i]];

    ### This way, 'j' is both the index for the UID vector and
    ### for the corresponding line in the sources
    nrOfMergedUtteranceCodings <- 0;
    for (j in seq_along(primarySourceUids[[i]])) {

      currentUID <- primarySourceUids[[i]][j];

      if (getOption('rock.debug', FALSE)) {
        cat0("\n     - Processing UID '", currentUID, "'.");
      }

      if (currentUID %in% allCodedUtterances) {
        nrOfMergedUtteranceCodings <-
          nrOfMergedUtteranceCodings + 1;
        ### Check whether one of them is already applied
        codings <-
          unname(unlist(parsedSources$utterances[[currentUID]]));

        if (getOption('rock.debug', FALSE)) {
          cat0("\n       - Found codings: ", vecTxtQ(codings), " in utterance:\n",
               "         '", mergedSources[[i]][j], "'");
        }

        alreadyAppliedCodings <-
          unlist(lapply(codings,
                        grepl,
                        x = mergedSources[[i]][j],
                        fixed=TRUE));

        if (getOption('rock.debug', FALSE)) {
          cat0("\n       - Already applied codings: ", vecTxtQ(codings[alreadyAppliedCodings]), ".");
        }

        ### Add new codes
        mergedSources[[i]][j] <-
          paste0(mergedSources[[i]][j], " ",
                 paste0(codings[!alreadyAppliedCodings],
                        collapse = " "));

        if (getOption('rock.debug', FALSE)) {
          cat0("\n       - Added codings: ", vecTxtQ(codings[!alreadyAppliedCodings]), ".");
        }

      }
    }
    if (!silent) {
      cat0("\n   - ", nrOfMergedUtteranceCodings, " utterances merged.");
    }

    newFilename <-
      paste0(outputPrefix,
             sub("^(.*)\\.[a-zA-Z0-9]+$",
                 "\\1",
                 basename(i)),
             outputSuffix,
             ".rock");
    if (tolower(output) == "same") {
      newFileDir <-
        dirname(i);
    } else {
      newFileDir <-
        output;
    }
    newFullname <- file.path(newFileDir,
                             newFilename);

    if (!silent) {
      cat0("\nStarting to write merged source '", newFilename,
           "' to directory '", newFileDir, "'.\n");
    }

    if (file.exists(newFullname) && (preventOverwriting)) {
      if (!silent) {
        message("Output file '", newFilename, "' already exists and ",
                "`preventOverwriting` is set to TRUE - not writing output file!");
      }
    } else {
      con <- file(description=newFullname,
                  open="w",
                  encoding=encoding);
      writeLines(text=mergedSources[[i]],
                 con=con);
      close(con);
    }

  }

  res <- list(parsedSources = parsedSources,
              primarySources = primarySources,
              mergedSources = mergedSources);

  return(invisible(res));

}
