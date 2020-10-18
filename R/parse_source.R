#' Parsing sources
#'
#' These function parse one (`parse_source`) or more (`parse_sources`) sources and the
#' contained identifiers, sections, and codes.
#'
#' @param text,file As `text` or `file`, you can specify a `file` to read with
#' encoding `encoding`, which will then be read using [base::readLines()]. If the
#' argument is named `text`, whether it is the path to an existing file is checked
#' first, and if it is, that file is read. If the argument is named `file`, and it
#' does not point to an existing file, an error is produced (useful if calling
#' from other functions). A `text` should be a character vector where every
#' element is a line of the original source (like provided by [base::readLines()]);
#' although if a character vector of one element *and* including at least one
#' newline character (`\\n`) is provided as `text`, it is split at the newline
#' characters using [base::strsplit()]. Basically, this behavior means that the
#' first argument can be either a character vector or the path to a file; and if
#' you're specifying a file and you want to be certain that an error is thrown if
#' it doesn't exist, make sure to name it `file`.
#' @param path The path containing the files to read.
#' @param extension The extension of the files to read; files with other extensions will
#' be ignored. Multiple extensions can be separated by a pipe (`|`).
#' @param regex Instead of specifing an extension, it's also possible to specify a regular
#' expression; only files matching this regular expression are read. If specified, `regex`
#' takes precedece over `extension`,
#' @param recursive Whether to also process subdirectories (`TRUE`)
#' or not (`FALSE`).
#' @param ignoreOddDelimiters If an odd number of YAML delimiters is encountered, whether this
#' should result in an error (`FALSE`) or just be silently ignored (`TRUE`).
#' @param encoding The encoding of the file to read (in `file`).
#' @param postponeDeductiveTreeBuilding Whether to imediately try to build the deductive
#' tree(s) based on the information in this file (`FALSE`) or whether to skip that. Skipping
#' this is useful if the full tree information is distributed over multiple files (in which case
#' you should probably call `parse_sources` instead of `parse_source`).
#' @param silent Whether to provide (`FALSE`) or suppress (`TRUE`) more detailed progress updates.
#' @param x The object to print.
#' @param prefix The prefix to use before the 'headings' of the printed result.
#' @param ... Any additional arguments are passed on to the default print method.
#'
#' @rdname parsing_sources
#' @aliases parsing_sources parse_source parse_sources print.rockParsedSource
#'
#' @examples ### Get path to example source
#' examplePath <-
#'   system.file("extdata", package="rock");
#'
#' ### Get a path to one example file
#' exampleFile <-
#'   file.path(examplePath, "example-1.rock");
#'
#' ### Parse single example source
#' parsedExample <- rock::parse_source(exampleFile);
#'
#' ### Show inductive code tree for the codes
#' ### extracted with the regular expression specified with
#' ### the name 'codes':
#' parsedExample$inductiveCodeTrees$codes;
#'
#' ### If you want `rock` to be chatty, use:
#' parsedExample <- rock::parse_source(exampleFile,
#'                                     silent=FALSE);
#'
#' ### Parse all example sources in that directory
#' parsedExamples <- rock::parse_sources(examplePath);
#'
#' ### Show combined inductive code tree for the codes
#' ### extracted with the regular expression specified with
#' ### the name 'codes':
#' parsedExamples$inductiveCodeTrees$codes;
#'
#' @export
parse_source <- function(text,
                         file,
                         ignoreOddDelimiters=FALSE,
                         postponeDeductiveTreeBuilding = FALSE,
                         encoding=rock::opts$get(encoding),
                         silent=rock::opts$get(silent)) {

  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);
  autoGenerateIds <- rock::opts$get(autoGenerateIds);
  persistentIds <- rock::opts$get(persistentIds);
  noCodes <- rock::opts$get(noCodes);
  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);
  attributeContainers <- rock::opts$get(attributeContainers);
  codesContainers <- rock::opts$get(codesContainers);
  delimiterRegEx <- rock::opts$get(delimiterRegEx);
  ignoreRegex <- rock::opts$get(ignoreRegex);

  if (missing(file)) {
    if (missing(text)) {
      stop("Provide either a `file` or a `text` to scan!");
    } else {
      if ((length(text) == 1) && file.exists(text)) {
        x <- readLines(text,
                       encoding=encoding,
                       warn=FALSE);
        if (!silent) {
          cat0("Read the contents of file '", text, "' (", length(x), " lines read).\n");
        }
      } else {
        x <- text;
        if ((length(x) == 1) && grepl('\n', x)) {
          x <-
            strsplit(x,
                     "\n")[[1]];
        }
        if (!silent) {
          cat0("Read input string (", length(x), " lines read).\n\n");
        }
      }
    }
  } else {
    if (file.exists(file)) {
      x <- readLines(file,
                     encoding=encoding,
                     warn=FALSE);
      if (!silent) {
        cat0("Read the contents of file '", file, "' (", length(x), " lines read).\n");
      }
    } else {
      stop("The file you specified in argument `file` ('",
           paste0(file, collapse=" "),
           "') does not exist. If you meant to provide a text ",
           "to process, please use argument `text`");
    }
  }

  ### Store results in the object to return
  res <-
    structure(list(arguments = as.list(environment())),
              class="rockParsedSource");

  ### First process YAML fragments and remove them
  res$yamlFragments <-
    yum::extract_yaml_fragments(text=x,
                                delimiterRegEx=delimiterRegEx,
                                ignoreOddDelimiters=ignoreOddDelimiters);
  x <-
    yum::delete_yaml_fragments(text=x,
                               delimiterRegEx=delimiterRegEx,
                               ignoreOddDelimiters=ignoreOddDelimiters);

  ### Process metadata and deductive code trees
  if (!is.null(res$yamlFragments)) {
    if (!silent) {
      cat0("Encountered YAML fragments. Parsing them for metadata.\n");
    }
    res$metadata <-
      yum::load_and_simplify(yamlFragments=res$yamlFragments,
                             select=paste0(attributeContainers, collapse="|"));
    if (!silent) {
      cat0("Read ", length(unlist(res$metadata)),
                " metadata specifications. Continuing with deductive code trees.\n");
    }
    res$rawDeductiveCodes <-
      yum::load_and_simplify(yamlFragments=res$yamlFragments,
                             select=paste0(codesContainers, collapse="|"));
    ### Get all deductive code ids
    res$deductiveCodes <-
      get_deductive_code_values(res$rawDeductiveCodes,
                                name="id");
    ### Remove empty codes
    res$deductiveCodes <-
      res$deductiveCodes[nchar(res$deductiveCodes)>0];
    if (!silent) {
      cat0("Read ", length(res$deductiveCodes),
                " deductive codes (",
                vecTxtQ(res$deductiveCodes), ").\n");
    }
    ### Store tree, unless we should postpone that
    if (!postponeDeductiveTreeBuilding) {
      ### Build tree
      deductiveCodeTrees <-
        yum::build_tree(res$rawDeductiveCodes);
      deductiveCodeTrees$name <- 'codes';
      res$deductiveCodeTrees <-
        deductiveCodeTrees;
    } else {
      deductiveCodeTrees <- NA;
    }
  } else {
    res$deductiveCodes <- NA;
    res$deductiveCodeTrees <- NA;
  }

  if (length(res$metadata) > 0) {

    ### Simplify YAML metadata and convert into a data frame
    res$metadataDf <-
      do.call(rbind,
              lapply(res$metadata,
                     as.data.frame,
                     stringsAsFactors=FALSE));

    ### Store metadata variables for convenient use later on
    res$convenience <-
      list(metadataVars =
             setdiff(names(res$metadataDf),
                     c(names(idRegexes),
                       names(attributeContainers))));

  } else {

    res$metadataDf <- data.frame();

    res$convenience <-
      list(metadataVars = NULL);

  }


  ### Then remove lines to ignore
  linesToIgnore <- grepl(ignoreRegex,
                         x);
  ignoredLines <- x[linesToIgnore];
  x <- x[!linesToIgnore];

  ### Create dataframe for parsing
  sourceDf <- data.frame(utterances_raw = x,
                         stringsAsFactors=FALSE);

  ### Identify sections
  if (!is.null(sectionRegexes) && length(sectionRegexes) > 0) {
    for (sectionRegex in names(sectionRegexes)) {
      ### Store whether each utterance matches
      sourceDf[, glue::glue("{sectionRegex}_match")] <-
        grepl(sectionRegexes[sectionRegex], x);
      ### Set incremental counter for each match
      if (glue::glue("{sectionRegex}_match") %in% names(sourceDf) &&
          (length(sourceDf[, glue::glue("{sectionRegex}_match")]) > 0)) {
        sourceDf[, glue::glue("{sectionRegex}_counter")] <-
          purrr::accumulate(sourceDf[, glue::glue("{sectionRegex}_match")],
                            `+`);
      }
    }
  }

  ### Process identifiers
  if (!is.null(idRegexes) && length(idRegexes) > 0) {
    for (idRegex in names(idRegexes)) {

      ### Get a list of matches
      ids <-
        regmatches(x,
                   gregexpr(idRegexes[idRegex], x));

      ### Check whether there are multiple matches
      multipleIds <-
        which(unlist(lapply(ids, length))>1);
      if (length(multipleIds) > 0) {
        warning(glue::glue("Multiple identifiers matching '{idRegex}' found in the following utterances:\n",
                       paste0(x[multipleIds],
                              collapse="\n"),
                       "\n\nOnly using the first identifier for each utterance, removing and ignoring the rest!"));
        ids <-
          lapply(ids, utils::head, 1);
      }

      ### Clean identifiers (i.e. only retain identifier content itself)
      ids <-
        lapply(ids, gsub, pattern=idRegexes[idRegex], replacement="\\1");

      ### Set "no_id" for utterances without id
      ids <-
        ifelse(unlist(lapply(ids,
                             length)),
               ids,
               "no_id");

      ### Convert from a list to a vector
      ids <- unlist(ids);

      if (length(ids) > 1) {
        ### Implement 'identifier persistence' by copying the
        ### identifier of the previous utterance if the identifier
        ### is not set - can't be done using vectorization as identifiers
        ### have to carry over sequentially.
        if (idRegex %in% persistentIds) {
          rawIds <- ids;
          for (i in 2:length(ids)) {
            if ((ids[i] == "no_id")) {
              ids[i] <- ids[i-1];
            }
          }
        }
      } else {
        ids = "no_id";
      }

      ### Check whether any matches were found
      if (!(all(ids=="no_id"))) {
        ### Generate identifiers for ids without identifier
        if (idRegex %in% autoGenerateIds) {
          ids[ids=="no_id"] <-
            paste0("autogenerated_id_",
                   1:(sum(ids=="no_id")));
        }
        ### Store identifiers in sourceDf
        sourceDf[, idRegex] <-
          ids;
        if (idRegex %in% persistentIds) {
          sourceDf[, paste0(idRegex, "_raw")] <-
            rawIds;
        }
      }
    }
  }

  ### Delete identifiers and store clean version in sourceDf
  x <-
    gsub(paste0(idRegexes, collapse="|"),
         "",
         x);
  sourceDf$utterances_without_identifiers <- x;

  ###---------------------------------------------------------------------------
  ### Process codes
  ###---------------------------------------------------------------------------

  codings <- list();
  codeProcessing <- list();
  occurrences <- list();
  occurrenceCounts <- list();
  namedOccurrences <- list();

  ### Process codes
  if (!is.null(codeRegexes) && length(codeRegexes) > 0) {

    for (codeRegex in names(codeRegexes)) {

      ### Find matches (the full substrings that match this code in each line)
      matches <-
        regmatches(x,
                   gregexpr(codeRegexes[codeRegex], x));

      ### Retain only the 'parenthesized' expression (i.e. the part of
      ### this code's regex between the parentheses, i.e., the actual code itself)
      cleanedMatches <-
        lapply(matches, gsub, pattern=codeRegexes[codeRegex], replacement="\\1");

      ### Remove all codes matching the 'noCodes' argument
      cleanedMatches <-
        lapply(cleanedMatches, grep, pattern=noCodes, value=TRUE, invert=TRUE);

      ### Get a complete list of all used codes. Note that this list can still contain
      ### duplicate leaves, because the ancestors are included here as well.
      codings[[codeRegex]] <-
        sort(unique(unlist(cleanedMatches)));

      ### Split these unique codes (but potentially containing duplicates given the
      ### inclusion of ancestors) into levels in case inductive coding was applied
      if ((nchar(inductiveCodingHierarchyMarker) > 0) &&
          (!is.null(codings[[codeRegex]])) &&
          (length(codings[[codeRegex]]) > 0)) {

        ### Create an object with the intermediate objects
        codeProcessing[[codeRegex]] <- list();

        ### Split the codes using the specified marker
        codeProcessing[[codeRegex]]$splitCodings <-
          strsplit(codings[[codeRegex]],
                   inductiveCodingHierarchyMarker);

        ### Make a specific vector with the leaves
        codeProcessing[[codeRegex]]$leafCodes <-
          unlist(lapply(codeProcessing[[codeRegex]]$splitCodings,
                        utils::tail,
                        1));

        ### Remove duplicate elements from the list with leaves
        codeProcessing[[codeRegex]]$leafCodes <-
          sort(unique(codeProcessing[[codeRegex]]$leafCodes));

        ### Make a separate list containing only the inductive codes
        codeProcessing[[codeRegex]]$inductiveCodes <-
          lapply(codeProcessing[[codeRegex]]$splitCodings,
                 function(codeVector) {
                   ### Check which of the codes in this vector are
                   ### deductive codes
                   deductives <-
                     codeVector %in% res$deductiveCodes;

                   if (all(deductives)) {
                     ### If no inductive codes included in this vector, return NULL
                     return(NULL);
                   } else if (all(!(deductives))) {
                     ### If they're all inductive codes, return them all
                     return(codeVector);
                   } else {
                     ### If a part is inductive, strip the deductive codes
                     if (identical(rev(sort(deductives)), deductives)) {
                       ### So we have 1+ deductive codes. We need to retain the
                       ### last one, but can remove its ancestors.
                       inductives <- !deductives;
                       inductives[max(which(deductives))] <- TRUE;
                       return(codeVector[inductives]);
                     } else {
                       problemCode <-
                         paste0("[[",
                                paste0(codeVector,
                                       sep=inductiveCodingHierarchyMarker),
                                "]]");
                       stop("I encountered a code specification that includes both deductive ",
                            "and inductive codes (", problemCode,
                            ", occurring on lines ",
                            grep(problemCode, res$arguments$x), " of the input source), but the inductive codes ",
                            "are not all descendents of the deductive codes! Inductive coding ",
                            "can be combined with deductive coding, but only if the inductive ",
                            "codes further specify (i.e. are subcodes, or descendents, of) the ",
                            "deductive codes.");
                     }
                   }
                 });

        ### Remove elements that are NULL
        codeProcessing[[codeRegex]]$inductiveCodes <-
          codeProcessing[[codeRegex]]$inductiveCodes[!unlist(lapply(codeProcessing[[codeRegex]]$inductiveCodes,
                                                                    is.null))];

      } else {
        codeProcessing[[codeRegex]] <-
          list(splitCodings = lapply(codings[[codeRegex]],
		                             function(x) return(x)),
               leafCodes = codings[[codeRegex]],
               inductiveCodes = codings[[codeRegex]][!(codings[[codeRegex]] %in% res$deductiveCodes)]);
      }

      ### Get inductive leaf codes
      codeProcessing[[codeRegex]]$inductiveLeafCodes <-
        unlist(lapply(codeProcessing[[codeRegex]]$splitCodings,
                      utils::tail,
                      1));
      codeProcessing[[codeRegex]]$inductiveLeafCodes <-
        sort(unique(codeProcessing[[codeRegex]]$inductiveLeafCodes));

      ### If inductive coding was applied using a hierarchical structure,
      ### build the inductive code tree
      if ((nchar(inductiveCodingHierarchyMarker) > 0) &&
          (!is.null(codeProcessing[[codeRegex]]$inductiveCodes)) &&
          (length(codeProcessing[[codeRegex]]$inductiveCodes) > 0)) {

        ### Process inductive code trees
        codeProcessing[[codeRegex]]$inductiveCodeTrees <-
          inductiveCodes_to_tree(inductiveCodes=codeProcessing[[codeRegex]]$inductiveCodes,
                                 silent=silent);

        ### Set graph style
        data.tree::SetGraphStyle(codeProcessing[[codeRegex]]$inductiveCodeTrees,
                                 directed="false");
        data.tree::SetGraphStyle(codeProcessing[[codeRegex]]$inductiveCodeTrees,
                                 rankdir = "LR");

        ### Try to convert to a DiagrammeR graph
        tryCatch({
          codeProcessing[[codeRegex]]$inductiveDiagrammeR <-
            data.tree::ToDiagrammeRGraph(codeProcessing[[codeRegex]]$inductiveCodeTrees);
        }, error = function(e) {
          warning("Error issued by 'data.tree::ToDiagrammeRGraph' when converting '",
                  codeRegex, "' code tree: ", e$message, "\n\nClass and content:\n\n",
                  paste0(utils::capture.output(print(class(codeProcessing[[codeRegex]]$inductiveCodeTrees))),
                         collapse="\n"),
                  "\n",
                  paste0(utils::capture.output(print(codeProcessing[[codeRegex]]$inductiveCodeTrees)),
                         collapse="\n"));
        });

        ### Set matches for lines that did
        ### not have a match to NA
        cleanedMatches[unlist(lapply(matches, length))==0] <- NA;

        ### Get presence of codes in utterances
        occurrences[[codeRegex]] <-
          lapply(get_leaf_codes(cleanedMatches,
                                inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker),
                 `%in%`,
                 x=codeProcessing[[codeRegex]]$leafCodes);

        ### Convert from logical to numeric
        occurrenceCounts[[codeRegex]] <-
          lapply(occurrences[[codeRegex]], as.numeric);

        ### Add the codes as names
        namedOccurrences[[codeRegex]] <-
          lapply(occurrenceCounts[[codeRegex]],
                 `names<-`,
                 value <- codeProcessing[[codeRegex]]$leafCodes);

        ### Convert the lists to dataframes
        sourceDf <-
          cbind(sourceDf,
                as.data.frame(do.call(rbind,
                                      namedOccurrences[[codeRegex]])));

        ### Delete codes from utterances
        x <-
          gsub(codeRegexes[codeRegex],
               "",
               x);

      }
    }
  }

  ### Trim spaces from front and back and store almost clean utterances
  sourceDf$utterances_clean_with_uids <-
    trimws(x);

  ### Extract and store UIDs
  sourceDf$uids <-
    ifelse(grepl(uidRegex,
                 sourceDf$utterances_clean_with_uids,
                 perl=TRUE),
           gsub(paste0(".*", uidRegex, ".*"),
                       "\\1",
                       sourceDf$utterances_clean_with_uids),
           "");

  ### Store really clear utterances
  sourceDf$utterances_clean <-
    trimws(gsub(uidRegex,
                "",
                sourceDf$utterances_clean_with_uids));

  if (nrow(sourceDf) > 0) {
    sourceDf$originalSequenceNr <- 1:nrow(sourceDf);

    cleanSourceDf <-
      sourceDf[!grepl(paste0(sectionRegexes, collapse="|"),
                      x), ];

    cleanSourceDf <-
      cleanSourceDf[nchar(cleanSourceDf$utterances_clean)>0, ];
  } else {
    cleanSourceDf <- data.frame();
  }

  if (nrow(cleanSourceDf) > 0) {
    cleanSourceDf$sequenceNr <- 1:nrow(cleanSourceDf);
  }

  ### Store results in the object to return
  res$sourceDf <- cleanSourceDf;
  res$rawSourceDf <- sourceDf;
  res$codings <- codeProcessing[[codeRegex]]$leafCodes;
  res$rawCodings <- codings;
  res$codeProcessing <- codeProcessing;
  res$inductiveCodeTrees <- purrr::map(res$codeProcessing, "inductiveCodeTrees");
  res$inductiveDiagrammeRs <- purrr::map(res$codeProcessing, "inductiveDiagrammeR");

  ### Merge metadata with source dataframe
  if (length(res$metadata) > 0) {

    ### Merge metadata with source data
    res$mergedSourceDf <-
      merge(res$sourceDf,
            res$metadataDf);

  } else {

    res$mergedSourceDf <-
      res$sourceDf;

  }

  ### Check for identifier column existence and convert to character
  for (i in names(idRegexes)) {
    if (i %in% names(res$mergedSourceDf)) {
      res$mergedSourceDf[, i] <-
        as.character(res$mergedSourceDf[, i]);
    } else {
      res$mergedSourceDf[, i] <-
        rep("", nrow(res$mergedSourceDf));
    }
  }

  ### Add codings and leaves only to the convenience list
  res$convenience$codings <- sort(unique(unlist(res$codings)));
  res$convenience$codingLeaves <-
    sort(unique(unlist(get_leaf_codes(res$codings,
                                      inductiveCodingHierarchyMarker=inductiveCodingHierarchyMarker))));

  res$convenience$codingPaths <-
    stats::setNames(res$convenience$codings,
                    res$convenience$codingLeaves);

  if (length(res$convenience$codings) > 0) {
    ### Count how often each code was used
    res$countedCodings <-
      colSums(res$sourceDf[, res$convenience$codings]);

  } else {
    res$countedCodings <-
      NULL;
  }

  ### Store all available deductive codes in this source
  res$convenience$deductiveCodes <-
    res$deductiveCodes;

  ### Store all available inductive codes in this source
  res$convenience$inductiveSplitCodes <-
    c(lapply(res$codeProcessing,
             function(x) {
               return(x$inductiveCodes);
             }));
  res$convenience$inductiveCodes <-
    lapply(res$convenience$inductiveSplitCodes,
           function(x) {
             return(sort(unique(unname(unlist(x)))));
           });

  if (!postponeDeductiveTreeBuilding && ("Node" %in% class(res$deductiveCodeTrees))) {
    ### Merge inductive code tree into deductive code tree (currently only support
    ### for one deductive code tree)
    res$extendedDeductiveCodeTrees <-
      data.tree::Clone(res$deductiveCodeTrees);
    res$fullyMergedCodeTrees <-
      data.tree::Clone(res$deductiveCodeTrees);

    for (i in names(res$inductiveCodeTrees)) {
      if ("Node" %in% class(res$inductiveCodeTrees[[i]])) {
        for (j in names(res$inductiveCodeTrees[[i]]$children)) {
          if (j %in% res$deductiveCodes) {
            currentNode1 <-
              data.tree::FindNode(res$extendedDeductiveCodeTrees,
                                  j);
            currentNode2 <-
              data.tree::FindNode(res$fullyMergedCodeTrees,
                                  j);
            for (k in names(res$inductiveCodeTrees[[i]]$children[[j]]$children)) {
              currentNode1$AddChildNode(res$inductiveCodeTrees[[i]]$children[[j]]$children[[k]]);
              currentNode2$AddChildNode(res$inductiveCodeTrees[[i]]$children[[j]]$children[[k]]);
            }
          } else {
            res$fullyMergedCodeTrees$AddChildNode(res$inductiveCodeTrees[[i]]$children[[j]]);
          }
        }
      }
    }
  } else {
    res$extendedDeductiveCodeTrees <- NA;
    res$fullyMergedCodeTrees <- NA;
  }
  if (!silent) {
    cat("\n\n");
  }

  ### Return result
  return(res);

}

#' @rdname parsing_sources
#' @method print rockParsedSource
#' @export
print.rockParsedSource <- function(x, prefix="### ",  ...) {
  totalSectionMatches <-
    sum(unlist(lapply(x$rawSourceDf[, grep('_match',
                                           names(x$rawSourceDf))],
                      as.numeric)));

  appliedCodes <-
    sort(unique(unlist(x$codings)));

  totalCodingMatches <-
    sum(unlist(x$sourceDf[, appliedCodes]));

  if (totalCodingMatches > 0) {
    codingInfo <-
      glue::glue("These {nrow(x$sourceDf)} utterances were coded ",
                 "{totalCodingMatches} times in total using these codes: ",
                 "{vecTxtQ(appliedCodes)}.");
  } else {
    codingInfo <-
      glue::glue("These {nrow(x$sourceDf)} utterances were not coded at all.");
  }

  if (length(x$inductiveCodeTrees) > 0) {
    inductiveTreesInfo <-
      glue::glue("This source contained inductive coding trees. ",
                 "These are shown in R Studio's viewer.\n\n")
  } else {
    inductiveTreesInfo <-
      glue::glue("This source contained no inductive coding trees.\n\n")
  }

  if (length(x$deductiveCodeTrees) > 0) {
    deductiveTreesInfo <-
      glue::glue("This source contained deductive coding trees. ",
                 "These are also shown in R Studio's viewer.\n\n")
  } else {
    deductiveTreesInfo <-
      glue::glue("This source contained no deductive coding trees.\n\n")
  }

  identifiers <-
    names(x$arguments$idRegexes);
  occurringIdentifiers <-
    identifiers[identifiers %in% names(x$sourceDf)];

  if (length(occurringIdentifiers) > 0) {
    actualIdentifiers <-
      lapply(x$sourceDf[, occurringIdentifiers, drop=FALSE],
             unique);
    actualIdentifiers <-
      lapply(actualIdentifiers,
             sort);
    actualIdentifiers <-
      lapply(actualIdentifiers,
             function(x) return(x[!(x=="no_id")]));
    identifierInfo <-
      glue::glue("This source contained matches with identifier regular expressions. Specifically, ",
                 glue::glue_collapse(lapply(names(actualIdentifiers),
                                            function(x) return(glue::glue("identifier regular expression '{x}' matched ",
                                                                          "with identifiers {vecTxtQ(actualIdentifiers[[x]])}"))),
                                     ", "),
                 ".");
  } else {
    identifierInfo <-
      glue::glue("This source contained no matches with identifier regular expressions.")
  }

  print(glue::glue("\n\n",
                   "{prefix}Preprocessing\n\n",
                   "The parsed source contained {length(x$arguments$x)} lines. ",
                   "After removing lines that matched '{x$arguments$ignoreRegex}', ",
                   "the regular expression specifying which lines to ignore, and did not ",
                   "make up the {length(x$yamlFragments)} YAML fragments with metadata or ",
                   "deductive coding tree specifications, {nrow(x$rawSourceDf)} lines remained.",
                   " {totalSectionMatches} of these matched one of the section regular ",
                   "expressions ({vecTxtQ(x$arguments$sectionRegexes)}), and after ",
                   " removing these lines and all lines that were empty after removing ",
                   " characters that matched one or more identifier ",
                   "({vecTxtQ(x$arguments$idRegexes)}) and coding ",
                   "({vecTxtQ(x$arguments$codeRegexes)}) regular expressions, ",
                   "{nrow(x$sourceDf)} utterances remained.",
                   "\n\n",
                   "{prefix}Identifiers\n\n",
                   identifierInfo,
                   "\n\n",
                   "{prefix}Utterances and coding\n\n",
                   codingInfo,
                   "\n\n",
                   "{prefix}Inductive coding trees\n\n",
                   inductiveTreesInfo,
                   "\n",
                   "{prefix}Deductive coding trees\n\n",
                   deductiveTreesInfo));
  #if (length(x$inductiveCodeTrees) > 0) {
  if (length(x$inductiveDiagrammeRs) > 0) {
    #for (i in names(x$inductiveCodeTrees)) {
    for (i in names(x$inductiveDiagrammeRs)) {
      #print(graphics::plot(x$inductiveCodeTrees[[i]]));
      print(DiagrammeR::render_graph(x$inductiveDiagrammeRs[[i]]));
    }
  }
  if (length(x$deductiveCodeTrees) > 0) {
    print(graphics::plot(x$deductiveCodeTrees));
  }
  invisible(x);
}
