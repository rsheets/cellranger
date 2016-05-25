#' cell_addr class
#'
#' The \code{cell_addr} class is used to hold the absolute row and column
#' location for one or more cells. An object of class \code{cell_addr} is a list
#' with two components of equal length, named \code{row} and \code{col},
#' consisting of integers greater than or equal to one or \code{NA}. This is in
#' contrast to the \code{\link{ra_ref}} class, which holds a representation of a
#' single absolute, relative, or mixed cell reference from, e.g., a formula.
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
  ## this way we don't have to require NA_integer_ which is annoying
  ## integer conversion was going to happen anyway
  row <- as.integer(row)
  col <- as.integer(col)
  stopifnot(length(row) > 0, length(col) > 0)
  if (length(row) > 1 && length(col) > 1) {
    stopifnot(length(row) == length(col))
  } else {
    n <- max(length(row), length(col))
    row <- rep_len(row, n)
    col <- rep_len(col, n)
  }
  neg <- isTRUE_v(row < 1) | isTRUE_v(col < 1)
  if (any(neg)) {
    ## data.frame > tibble here because want original row names (number, here)
    out <- data.frame(row, col)[neg, ,drop = FALSE]
    printed_x <- utils::capture.output(print(out))
    stop("cell_addr objects require absolute row and column, must be >= 1:\n",
         paste(printed_x, collapse = "\n"), call. = FALSE)
  }
  structure(list(row = row, col = col), class = "cell_addr")
}

#' @export
print.cell_addr <- function(x, ..., n = NULL) {
  cat("<cell_addr:", length(x), "cells>\n")
  print(tibble::trunc_mat(tibble::as_data_frame(unclass(x)), n = n))
  cat("\n")
  invisible(x)
}

#' @export
`[.cell_addr` <-
  function(x, i) cell_addr(row = addr_row(x)[i], col = addr_col(x)[i])

#' @export
`[[.cell_addr` <-
  function(x, i) cell_addr(row = addr_row(x)[[i]], col = addr_col(x)[[i]])

#' @export
length.cell_addr <- function(x) length(addr_row(x))

#' Get row from cell location or reference
#'
#' @param x a suitable representation of cell(s) or a cell area reference
#' @template param-ddd
#'
#' @return integer vector
#' @export
addr_row <- function(x, ...) UseMethod("addr_row")

#' Get column from cell location or reference
#'
#' @param x a suitable representation of cell(s) or a cell area reference
#' @template param-ddd
#'
#' @return integer vector
#' @export
addr_col <- function(x, ...) UseMethod("addr_col")

#' @describeIn addr_row Method for \code{\link{cell_addr}} objects
#' (ca <- cell_addr(1:4, 3))
#' addr_row(ca)
#' @export
addr_row.cell_addr <- function(x, ...) x$row

#' @describeIn addr_col Method for \code{\link{cell_addr}} objects
#' (ca <- cell_addr(1:4, 3))
#' addr_col(ca)
#' @export
addr_col.cell_addr <- function(x, ...) x$col

#' Convert to a cell_addr object
#'
#' Convert various representations of a cell reference into an object of class
#' \code{\link{cell_addr}}. Recall that \code{\link{cell_addr}} objects hold
#' absolute row and column location, so \code{\link{ra_ref}} objects or cell
#' reference strings with relative or mixed references will raise a warning and
#' generate \code{NA}s.
#'
#' @param x a cell reference
#' @template param-ddd
#'
#' @return a \code{\link{cell_addr}} object
#' @name as.cell_addr
NULL

#' @rdname as.cell_addr
#' @export
as.cell_addr <- function(x, ...) UseMethod("as.cell_addr")

#' @rdname as.cell_addr
#' @export
as.cell_addr_v <- function(x, ...) UseMethod("as.cell_addr_v")

#' @rdname as.cell_addr
#' @export
#' @examples
#' as.cell_addr(ra_ref())
#' rar <- ra_ref(2, TRUE, 5, TRUE)
#' as.cell_addr(rar)
#' ## mixed reference
#' rar <- ra_ref(2, FALSE, 5, TRUE)
#' as.cell_addr(rar)
as.cell_addr.ra_ref <- function(x, ...) {
  if (!isTRUE(x$row_abs) || !isTRUE(x$col_abs)) {
    warning("Non-absolute references found ... NAs generated", call. = FALSE)
    if (!isTRUE(x$row_abs)) {
      x$row_ref <- NA
    }
    if (!isTRUE(x$col_abs)) {
      x$col_ref <- NA
    }
  }
  cell_addr(row = x$row_ref, col = x$col_ref)
}

#' @rdname as.cell_addr
#' @examples
#' ra_ref_list <-
#'   list(ra_ref(), ra_ref(2, TRUE, 5, TRUE), ra_ref(2, FALSE, 5, TRUE))
#' as.cell_addr_v(ra_ref_list)
#' @export
as.cell_addr_v.list <- function(x, ...) {
  stopifnot(all(vapply(x, inherits, logical(1), what = "ra_ref")))
  ca_list <- lapply(x, as.cell_addr)
  cell_addr(row = vapply(ca_list, addr_row, integer(1)),
            col = vapply(ca_list, addr_col, integer(1)))
}

#' @rdname as.cell_addr
#' @template param-fo
#' @template param-strict
#' @export
#' @examples
#' as.cell_addr("$D$12")
#' as.cell_addr("R4C3")
#' as.cell_addr(c("$C$4", "$D$12"))
#' as.cell_addr("$F2")
#' as.cell_addr("R[-4]C3")
#' as.cell_addr("F2", strict = FALSE)
as.cell_addr.character <- function(x, fo = NULL, strict = TRUE, ...) {
  suppressWarnings(
    ## one warning is enough -- let as.cell_addr take care of it in next step
    ra_ref_list <- as.ra_ref_v(x, fo = fo, strict = strict)
  )
  as.cell_addr_v(ra_ref_list)
}

#' @rdname as.cell_addr
#' @export
as.cell_addr_v.character <- as.cell_addr.character
