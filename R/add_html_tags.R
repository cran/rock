#' Add HTML tags to a source
#'
#' This function adds HTML tags to a source to allow pretty printing/viewing.
#'
#' @inheritParams parsing_sources
#' @param x A character vector with the source
#' @param codeClass,idClass,sectionClass,uidClass,utteranceClass The classes
#' to use for, respectively, codes, identifiers (such as case identifiers or
#' coder identifiers), section breaks, utterance identifiers, and full
#' utterances. All `<span>` elements except for the full utterances, which
#' are placed in `<div>` elements.
#'
#' @return The character vector with the replacements made.
#' @export
#'
#' @examples add_html_tags("[[cid=participant1]]
#' This is something this participant may have said.
#' Just like this. [[thisIsACode]]
#' ---paragraph-break---
#' And another utterance.");
add_html_tags <- function(x,
                          codeClass = rock::opts$get(codeClass),
                          idClass = rock::opts$get(idClass),
                          sectionClass = rock::opts$get(sectionClass),
                          uidClass = rock::opts$get(uidClass),
                          utteranceClass = rock::opts$get(utteranceClass)) {

  codeRegexes <- rock::opts$get(codeRegexes);
  idRegexes <- rock::opts$get(idRegexes);
  sectionRegexes <- rock::opts$get(sectionRegexes);
  uidRegex <- rock::opts$get(uidRegex);
  inductiveCodingHierarchyMarker <- rock::opts$get(inductiveCodingHierarchyMarker);

  res <- x;

  ### First replace smaller than and bigger than symbols
  ### with the corresponding entities
  res <- gsub("<", "&lt;", res, fixed=TRUE);
  res <- gsub(">", "&gt;", res, fixed=TRUE);

  ### Also replace <> symbols in all codeRegexes
  codeRegexes <- gsub("<", "&lt;", codeRegexes, fixed=TRUE);
  codeRegexes <- gsub(">", "&gt;", codeRegexes, fixed=TRUE);

  ### Add html tags
  for (currentCodeRegexName in names(codeRegexes)) {
    currentCodeRegex <- codeRegexes[currentCodeRegexName];
    codeContentMatches <- grepl(currentCodeRegex, res);
    if (any(codeContentMatches)) {
      codeContent <-
        ifelse(codeContentMatches,
               gsub(paste0(".*", currentCodeRegex, ".*"),
                    "\\1",
                    res),
               "");
      splitCodeContent <-
        unlist(lapply(strsplit(codeContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeContent <-
        paste0('<span class="', codeClass,
               ' ', currentCodeRegexName,
               #' ', splitCodeContent,
               '">');
      # splitCodeContent <-
      #   ifelse(codeContentMatches,
      #          splitCodeContent,
      #          "");
      # currentCodeRegexMatches <-
      #   gregexpr(paste0("(", currentCodeRegex, ")"),
      #            res);
      res <-
        gsub(paste0("(", currentCodeRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res);
    }
  }

  ### Also replace <> symbols in all sectionRegexes
  sectionRegexes <- gsub("<", "&lt;", sectionRegexes, fixed=TRUE);
  sectionRegexes <- gsub(">", "&gt;", sectionRegexes, fixed=TRUE);

  ### Add break tags
  for (currentBreakRegexName in names(sectionRegexes)) {
    currentBreakRegex <- sectionRegexes[currentBreakRegexName];
    codeContentMatches <- grepl(currentBreakRegex, res);
    if (any(codeContentMatches)) {
      codeContent <-
        ifelse(codeContentMatches,
               gsub(paste0(".*", currentBreakRegex, ".*"),
                    "\\1",
                    res),
               "");
      splitCodeContent <-
        unlist(lapply(strsplit(codeContent,
                               inductiveCodingHierarchyMarker),
                      paste0,
                      collapse=" "));
      splitCodeContent <-
        paste0('<span class="', sectionClass,
               ' ', currentBreakRegexName,
               '">');
      res <-
        gsub(paste0("(", currentBreakRegex, ")"),
             paste0(splitCodeContent, '\\1</span>'),
             res);
    }
  }

  ### Also replace <> symbols in all idRegexes
  idRegexes <- gsub("<", "&lt;", idRegexes, fixed=TRUE);
  idRegexes <- gsub(">", "&gt;", idRegexes, fixed=TRUE);

  ### Add identifier tags
  for (currentIdRegexName in names(idRegexes)) {
    currentIdRegex <- idRegexes[currentIdRegexName];
    codeContentMatches <- grepl(currentIdRegex, res);
    if (any(codeContentMatches)) {
      res <-
        gsub(paste0("(", currentIdRegex, ")"),
             paste0('<span class="', idClass,
                    ' ', currentIdRegexName,
                    '">\\1</span>'),
             res);
    }
  }

  ### Add UID tags
  res <-
    gsub(paste0("(", uidRegex, ")"),
         paste0('<span class="', uidClass,
                '">\\1</span>'),
         res);

  res <- paste0('<div class="', utteranceClass, '">', res, '</div>\n');

  return(res);

}
