#' cell_addr class
#'
#' The \code{cell_addr} class is used to hold the absolute row and column
#' location for a single cell. This is in contrast to the \code{\link{ra_ref}}
#' class, which holds a representation of a single absolute, relative, or mixed
#' cell reference from, e.g., a formula.
#'
#' @param row integer, row, must be greater than or equal to 1
#' @param col integer, column, must be greater than or equal to 1
#'
#' @return a \code{cell_addr} object
#' @export
#'
#' @template reference-sestoft
#'
#' @examples
#' cell_addr(4, 3)
cell_addr <- function(row, col) {
  stopifnot(length(row) == 1L, length(col) == 1L,
            is.numeric(row), is.numeric(col))
  row <- as.integer(row)
  col <- as.integer(col)
  if (row < 1 || col < 1) {
    stop("cell_addr objects give absolute row and column, must be >= 1:\n",
         " row = ", row, ", col = ", col, "\n", call. = FALSE)
  }
  structure(c(row = row, col = col), class = "cell_addr")
}

#' @export
print.cell_addr <- function(x, ...) {
  cat("<cell_addr>\n")
  print(unclass(x), ...)
}

#' @describeIn as.ra_ref Convert a \code{cell_addr} into a \code{\link{ra_ref}}
#' @export
#' @examples
#' ca <- cell_addr(2, 5)
#' as.ra_ref(ca)
as.ra_ref.cell_addr <- function(x, ...) {
  ra_ref(rowRef = x["row"], rowAbs = TRUE, colRef = x["col"], colAbs = TRUE)
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
#' @export
to_string.cell_addr <- function(x, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  to_string(as.ra_ref(x), fo = fo)
}

#' Convert to a cell_addr object
#'
#' Convert various representations of a cell reference into an object of class
#' \code{\link{cell_addr}}. Recall that \code{\link{cell_addr}} objects hold the
#' absolute row and column of a single cell, so not all \code{\link{ra_ref}}
#' objects or cell reference strings can be successfully converted.
#'
#' @param x a cell reference
#' @param ... other arguments passed along to methods
#'
#' @return a \code{\link{cell_addr}} object
#'
#' @export
as.cell_addr <- function(x, ...) UseMethod("as.cell_addr")

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

#' @describeIn as.cell_addr Convert a string representation of an absolute cell
#'   reference into a \code{cell_addr} object
#' @export
#' @examples
#' as.cell_addr("$D$12")
#' as.cell_addr("$C$4")
#' as.cell_addr("R4C3")
#'
#' \dontrun{
#' # none of these will work
#' as.cell_addr("$F2")
#' as.cell_addr("R[-4]C3")
#' }
as.cell_addr.character <- function(x, ...) {
  as.cell_addr(as.ra_ref(x))
}
