#' Convert a document (.docx, .pdf, etc) to a plain text file
#'
#' This is a thin wrapper around [textreadr::read_document()] that also
#' writed the result to `output`, doing its best to correctly write UTF-8
#' (based on the approach recommended in [this blog post](
#' https://tomizonor.wordpress.com/2013/04/17/file-utf8-windows/)).
#'
#' @param input The path to the input file.
#' @param output The path and filename to write to. If this is a path to
#' an existing directory (without a filename specified), the `input` filename
#' will be used, and the extension will be replaced with `extension`.
#' @param encoding The encoding to use when writing the text file.
#' @param newExt The extension to append: only used if `output = NULL` and
#' `newExt` is not `NULL`, in which case the output will be written to a file
#' with the same name as `input` but with `newExt` as extension.
#' @param preventOverwriting Whether to prevent overwriting existing files.
#' @param silent Whether to the silent or chatty.
#' @param skip The number of lines to skip (see [textreadr::read_document()]).
#' @param remove.empty If `TRUE` empty elements in the vector are
#' removed (see [textreadr::read_document()]).
#' @param trim If `TRUE` the leading/training white space is
#' removed (see [textreadr::read_document()]).
#' @param combine If `TRUE` the vector is concatenated into a single string
#' `textshape::combine()`. (see [textreadr::read_document()]).
#' @param format For .doc files only. Logical. If `TRUE` the output will keep
#' doc formatting (e.g., bold, italics, underlined). This corresponds to
#' the `-f` flag in antiword (see [textreadr::read_document()]).
#' @param ocr If `TRUE` .pdf documents with a non-text pull using
#' `pdftools::pdf_text()` will be re-run using OCR via the `tesseract::ocr()`
#' function. This will create temporary .png files and will require a much
#' larger compute time (see [textreadr::read_document()]).
#' @param ... Other arguments passed to [textreadr::read_pdf()],
#' [textreadr::read_html()], [textreadr::read_docx()], [textreadr::read_doc()],
#' or [base::readLines()] (by [textreadr::read_document()]).
#'
#' @return The converted source, as a character vector.
#' @export
#'
#' @examples print(
#'   rock::doc_to_txt(
#'     input = system.file(
#'       "extdata/doc-to-test.docx", package="rock"
#'     )
#'   )
#' );
doc_to_txt <- function(input,
                       output = NULL,
                       encoding = rock::opts$get("encoding"),
                       newExt = NULL,
                       preventOverwriting = rock::opts$get("preventOverwriting"),
                       silent = rock::opts$get("silent"),
                       skip = 0,
                       remove.empty = TRUE,
                       trim = TRUE,
                       combine = FALSE,
                       format = FALSE,
                       ocr = TRUE,
                       ...
                      ) {

  msg("Reading input file from '", input, "'.\n",
      silent = silent);

  res <-
    textreadr::read_document(
      file = input,
      skip = skip,
      remove.empty = remove.empty,
      trim = trim,
      combine = combine,
      format = format,
      ocr = ocr,
      ... = ...
    );

  if ((is.null(output)) && (!is.null(newExt))) {
    output <-
      paste0(
        tools::file_path_sans_ext(
          input
        ),
        ".",
        newExt
      );
  } else if ((!is.null(output)) &&
             (dir.exists(output)) &&
             (!is.null(newExt))) {

    output <-
      file.path(
        output,
        paste0(
          basename(
            tools::file_path_sans_ext(
              input
            )
          ),
          ".",
          newExt
        )
      );
  }

  if (!is.null(output)) {

    writingResult <-
      writeTxtFile(
        x = res,
        output = output,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent
      );

    if (!writingResult) {
      warning("Could not write output file to `",
              output, "`.");
    }

  }

  return(invisible(res));

}
