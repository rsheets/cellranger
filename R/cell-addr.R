#' cell_addr class
#'
#' The \code{cell_addr} class is used to hold the absolute row and column
#' location for one or more cells. This is in contrast to the
#' \code{\link{ra_ref}} class, which holds a representation of a single
#' absolute, relative, or mixed cell reference from, e.g., a formula.
#'
#' An object of class \code{cell_addr} is a list with two components of equal
#' length, named \code{row} and \code{col}, consisting of integers greater than
#' or equal to one.
#'
#' @param row integer. Must be the same length as \code{col} or of length one,
#'   which will be recycled to the length of \code{col}.
#' @param col integer. Same deal as for \code{row}.
#'
#' @return a \code{cell_addr} object
#' @export
#'
#' @template reference-sestoft
#'
#' @examples
#' cell_addr(4, 3)
#' (ca <- cell_addr(1:4, 3))
#' ca[2:3]
#' ca[[4]]
#' length(ca)
cell_addr <- function(row, col) {
  stopifnot(length(row) > 0L, length(col) > 0L,
            is.numeric(row), is.numeric(col))
  if (length(row) > 1 && length(col) > 1) {
    stopifnot(length(row) == length(col))
  } else {
    n <- max(length(row), length(col))
    row <- rep_len(row, n)
    col <- rep_len(col, n)
  }
  row <- as.integer(row)
  col <- as.integer(col)
  neg <- row < 1 | col < 1
  if (any(neg)) {
    ## data.frame's insistence on row names is actually nice here
    out <- data.frame(row, col)[neg, ,drop = FALSE]
    printed_x <- capture.output(print(out))
    stop("cell_addr objects require absolute row and column, must be >= 1:\n",
         paste(printed_x, collapse = "\n"), call. = FALSE)
  }
  structure(list(row = row, col = col), class = "cell_addr")
}

#' @export
print.cell_addr <- function(x, ...) {
  cat("<cell_addr>\n")
  ## data.frame is decidedly less charming here but will do for now
  print(as.data.frame(unclass(x)), ...)
}

#' @export
`[.cell_addr` <- function(x, i) cell_addr(row = x$row[i], col = x$col[i])

#' @export
`[[.cell_addr` <- function(x, i) cell_addr(row = x$row[[i]], col = x$col[[i]])

#' @export
length.cell_addr <- function(x) length(x$row)

#' Get row from cell location or reference
#'
#' @param x a suitable representation of cell(s) or a cell area reference
#' @param ... other arguments passed along to methods
#'
#' @return integer vector
#' @export
cell_row <- function(x, ...) UseMethod("cell_row")

#' Get column from cell location or reference
#'
#' @param x a suitable representation of cell(s) or a cell area reference
#' @param ... other arguments passed along to methods
#'
#' @return integer vector
#' @export
cell_col <- function(x, ...) UseMethod("cell_col")

#' @describeIn cell_row Method for \code{\link{cell_addr}} objects
#' (ca <- cell_addr(1:4, 3))
#' cell_row(ca)
#' @export
cell_row.cell_addr <- function(x, ...) x$row

#' @describeIn cell_col Method for \code{\link{cell_addr}} objects
#' (ca <- cell_addr(1:4, 3))
#' cell_col(ca)
#' @export
cell_col.cell_addr <- function(x, ...) x$col

#' Convert to a cell_addr object
#'
#' Convert various representations of a cell reference into an object of class
#' \code{\link{cell_addr}}. Recall that \code{\link{cell_addr}} objects hold the
#' absolute row and column of a single cell, so not all \code{\link{ra_ref}}
#' objects or cell reference strings can be successfully converted.
#'
#' @param x a cell reference
#' @param ... other arguments passed along to methods
#' @template param-fo
#'
#' @return a \code{\link{cell_addr}} object
#'
#' @export
as.cell_addr <- function(x, fo = NULL, ...) UseMethod("as.cell_addr")

#' @describeIn as.cell_addr Convert a \code{\link{ra_ref}} object into a
#'   \code{cell_addr} object
#' @export
#' @examples
#' as.cell_addr(ra_ref())
#' rar <- ra_ref(2, TRUE, 5, TRUE)
#' as.cell_addr(rar)
as.cell_addr.ra_ref <- function(x, ...) {
  if (!x$rowAbs || !x$colAbs) {
    stop("cell_addr objects give absolute row and column, not relative:\n",
         " rowAbs = ", x$rowAbs, ", rowRef = ", x$rowRef, "\n",
         " colAbs = ", x$colAbs, ", colRef = ", x$colRef, "\n",
         call. = FALSE)
  }
  cell_addr(row = x$rowRef, col = x$colRef)
}

#' @describeIn as.cell_addr Convert a string representation of absolute cell
#'   references into a \code{cell_addr} object
#' @export
#' @examples
#' as.cell_addr("$D$12")
#' as.cell_addr("$C$4")
#' as.cell_addr("R4C3")
#' as.cell_addr(c("R4C3", "$C$4", "$D$12"))
#'
#' \dontrun{
#' # none of these will work because they are relative references
#' as.cell_addr("$F2")
#' as.cell_addr("R[-4]C3")
#' }
as.cell_addr.character <- function(x, fo = NULL, ...) {
  ra_ref_list <- lapply(x, as.ra_ref, fo = fo)
  ca_list <- lapply(ra_ref_list, as.cell_addr)
  cell_addr(row = vapply(ca_list, cell_row, integer(1)),
            col = vapply(ca_list, cell_col, integer(1)))
}

#' @describeIn as.ra_ref Convert a \code{cell_addr} into a \code{\link{ra_ref}}
#' @export
#' @examples
#' ca <- cell_addr(2, 5)
#' as.ra_ref(ca)
#'
#' ca <- cell_addr(1:3, 1)
#' \dontrun{
#' ## won't work because as.ra_ref methods not natively vectorized
#' as.ra_ref(ca)
#' }
#' ## but it's easy enough to do with Vectorize
#' f <- Vectorize(as.ra_ref, USE.NAMES = FALSE, SIMPLIFY = FALSE)
#' f(ca)
as.ra_ref.cell_addr <- function(x, ...) {
  stopifnot(length(x) == 1L)
  ra_ref(rowRef = cell_row(x), rowAbs = TRUE,
         colRef = cell_col(x), colAbs = TRUE)
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
