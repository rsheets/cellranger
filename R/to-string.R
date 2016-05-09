#' Get string representation of cell references
#'
#' Convert various representations of a cell reference to character
#' \itemize{
#' \item \code{to_string} is not necessarily vectorized. For example, when the
#' the input is of class \code{\link{ra_ref}}, it must of be of length one.
#' However, to be honest, this will actually work for \code{\link{cell_addr}},
#' even when length > 1.
#' \item \code{to_string_v} is guaranteed to be vectorized. In particular, input
#' can be a \code{\link{cell_addr}} of length >= 1 or a list of
#' \code{\link{ra_ref}} objects.
#' }
#' If either the row or column reference is relative, note that, in general,
#' it's impossible to convert to an "A1" formatted string. We would have to know
#' "relative to what?".
#'
#' @param x a suitable representation of a cell or cell area reference: a single
#'   \code{\link{ra_ref}} object or a list of them or a \code{\link{cell_addr}}
#'   object
#' @template param-fo
#' @template param-strict
#' @template param-sheet
#' @template param-ddd
#'
#' @return a character vector
#' @name to_string
NULL

#' @rdname to_string
#' @export
to_string <-
  function(x, fo = c("R1C1", "A1"),
           strict = TRUE, sheet = NULL, ...) UseMethod("to_string")

#' @rdname to_string
#' @export
to_string_v <-
  function(x, fo = c("R1C1", "A1"),
           strict = TRUE, sheet = NULL, ...) UseMethod("to_string_v")

#' @rdname to_string
#' @examples
#' ## exactly one ra_ref --> string
#' to_string(ra_ref())
#' to_string(ra_ref(), fo = "A1")
#' to_string(ra_ref(), fo = "A1", strict = FALSE)
#' to_string(ra_ref(row_ref = 3, col_ref = 2))
#' to_string(ra_ref(row_ref = 3, col_ref = 2, sheet = "helloooo"))
#' (mixed_ref <- ra_ref(row_ref = 10, row_abs = FALSE, col_ref = 3))
#' to_string(mixed_ref)
#'
#' ## this will raise warning and generate NA, because row reference is
#' ## relative and format is A1
#' to_string(mixed_ref, fo = "A1")
#'
#' @export
to_string.ra_ref <- function(x, fo = c("R1C1", "A1"),
                             strict = TRUE, sheet = NULL, ...) {
  if (any(vapply(x[c("row_ref", "row_abs", "col_ref", "col_abs")],
                 is.na, logical(1)))) return(NA_character_)
  fo <- match.arg(fo)
  sheet <- sheet %||% !is.na(x$sheet)
  if (fo == "A1") {
    if (!isTRUE(x$row_abs) || !isTRUE(x$col_abs)) {
      warning("Only absolute references can be converted to an A1 formatted ",
              "string ... NAs generated", call. = FALSE)
      return(NA_character_)
    }
    if (!strict) {
      x <- relativize(x)
    }
    ref_string <-
      paste0(rel_abs_format(x$col_abs, fo = "A1"), num_to_letter(x$col_ref),
             rel_abs_format(x$row_abs, fo = "A1"), x$row_ref)
  } else {
    ref_string <- paste0("R", rel_abs_format(x$row_abs, x$row_ref),
                         "C", rel_abs_format(x$col_abs, x$col_ref))
  }
  if (sheet) {
    ref_string <- paste(add_single_quotes(x$sheet), ref_string, sep = "!")
  }
  ## no support to put file name in the string ... wait til I see it needed IRL
  ref_string
}

#' @rdname to_string
#' @examples
#' ## a list of ra_ref's --> character vector
#' ra_ref_list <-
#'   list(ra_ref(), ra_ref(2, TRUE, 5, TRUE), ra_ref(2, FALSE, 5, TRUE))
#' to_string_v(ra_ref_list)
#'
#' @export
to_string_v.list <- function(x, fo = c("R1C1", "A1"),
                             strict = TRUE, sheet = NULL, ...) {
  stopifnot(all(vapply(x, inherits, logical(1), what = "ra_ref")))
  vapply(x, to_string, character(1), fo = fo, strict = strict, sheet = sheet)
}

#' @rdname to_string
#' @examples
#' ## cell_addr --> string
#' (ca <- cell_addr(3, 8))
#' to_string(ca)
#' to_string(ca, fo = "A1")
#'
#' (ca <- cell_addr(1:4, 3))
#' to_string(ca)
#' to_string(ca, fo = "A1")
#' @export
to_string.cell_addr <- function(x, fo = c("R1C1", "A1"),
                                strict = TRUE, sheet = FALSE, ...) {
  fo <- match.arg(fo)
  ra_ref_list <- mapply(ra_ref, row_ref = addr_row(x), col_ref = addr_col(x),
                        SIMPLIFY = FALSE)
  vapply(ra_ref_list, to_string, character(1), fo = fo,
         strict = strict, sheet = sheet)
}

#' @rdname to_string
#' @examples
#' ## explicitly go from cell_addr, length > 1 --> character vector
#' (ca <- cell_addr(1:4, 3))
#' to_string_v(ca)
#' to_string_v(ca, fo = "A1")
#' @export
to_string_v.cell_addr <- to_string.cell_addr
