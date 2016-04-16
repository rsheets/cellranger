#' Get string representation of cell references
#'
#' If either the row or column reference is relative, note that, in general,
#' it's impossible to convert to an "A1" formatted string. We would have to know
#' "relative to what?".
#'
#' @param x a suitable representation of a cell or cell area reference
#' @template param-fo
#'
#' @return a character vector of length one
#' @export
to_string <- function(x, fo = c("R1C1", "A1")) UseMethod("to_string")

#' @describeIn to_string Convert a \code{\link{ra_ref}} object to a cell
#'   reference string
#' @examples
#' to_string(ra_ref())
#' to_string(ra_ref(), fo = "A1")
#' to_string(ra_ref(rowRef = 3, colRef = 2))
#' (rel_ref <- ra_ref(rowRef = 10, rowAbs = FALSE, colRef = 3))
#' to_string(rel_ref)
#' \dontrun{
#' ## this won't work because row reference is relative and format is A1
#' to_string(rel_ref, fo = "A1")
#' }
#' @export
to_string.ra_ref <- function(x, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  if (fo == "A1") {
    stopifnot_abs(x)
    return(paste0(rel_abs_format(x$colAbs, fo = "A1"), num_to_letter(x$colRef),
                  rel_abs_format(x$rowAbs, fo = "A1"), x$rowRef))
  }
  paste0("R", rel_abs_format(x$rowAbs, x$rowRef),
         "C", rel_abs_format(x$colAbs, x$colRef))
}

#' @describeIn to_string Convert a \code{\link{cell_addr}} object to a cell
#'   reference string
#'
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
to_string.cell_addr <- function(x, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  ra_ref_list <- mapply(ra_ref, rowRef = cell_row(x), colRef = cell_col(x),
                        SIMPLIFY = FALSE)
  vapply(ra_ref_list, to_string, character(1), fo = fo)
}
