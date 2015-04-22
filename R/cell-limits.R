#' Create a cell_limits object
#'
#' The expectation is that \code{as.cell_limits} will be called to process
#' user-provided input on the target cell range for read/write operations on a
#' spreadsheet. Downstream code can be written assuming cell limits are stored
#' in a valid \code{cell_limits} object.
#'
#' A \code{cell_limits} object is a list with two components:
#'
#' \itemize{
#' \item \code{rows} vector, of the form \code{c(min, max)}
#' \item \code{cols} vector, of the form \code{c(min, max)}
#' }
#'
#' Typically the \code{min} and \code{max} are positive integers, where the
#' first (the minimum) is less than or equal to the second (the maximum). A
#' value of \code{NA} means the corresponding limit is left unspecified.
#' Therefore a verbose way to specify no limits at all would be
#' \code{cell_limits(c(NA, NA), c(NA, NA))}.
#'
#' Spreadsheet ranges can be specified using "A1" notation or "R1C1" notation
#' and dollar signs will be ignored, i.e. "A$1:$B$32" is equivalent to "A1:B32".
#'
#' @param rows vector holding minimum and maximum row
#' @param cols vector holding minimum and maximum col
#' @param x input to convert into a \code{cell_limits} object
#'
#' @return a \code{cell_limits} object
#'
#' @examples
#' cell_limits(c(1, 3), c(1, 5))
#' cell_limits(c(NA, 7), c(3, NA))
#' cell_limits(c(NA, 7))
#' cell_limits(cols = c(NA, 7))
#'
#' dim(as.cell_limits("A1:F10"))
#' dim(cell_limits(cols = c(2, 5)))
#'
#' @export
cell_limits <- function(rows = c(NA_integer_, NA_integer_),
                        cols = c(NA_integer_, NA_integer_)) {

  stopifnot(length(rows) == 2L, length(cols) == 2L)

  rows <- as.integer(rows)
  cols <- as.integer(cols)

  NA_or_pos <- function(x) is.na(x) | x > 0
  stopifnot(all(NA_or_pos(rows)))
  stopifnot(all(NA_or_pos(rows)))

  if(!anyNA(rows)) stopifnot(rows[1] <= rows[2])
  if(!anyNA(cols)) stopifnot(cols[1] <= cols[2])

  structure(list(rows = rows, cols = cols), class = "cell_limits")

}

#' @export
print.cell_limits <- function(x, ...) {
  rows <- ifelse(is.na(x$rows), "-", as.character(x$rows))
  cols <- ifelse(is.na(x$cols), "-", as.character(x$cols))

  cat("<cell_limits (", rows[1], ", ", cols[1], ") x (",
      rows[2], ", ", cols[2], ")>\n",
      sep = "")
}

#' @rdname cell_limits
#' @export
dim.cell_limits <- function(x) vapply(x, diff, integer(1)) + 1L

#' @rdname cell_limits
#' @export
as.cell_limits <- function(x) UseMethod("as.cell_limits")

#' @rdname cell_limits
#' @export
as.cell_limits.cell_limits <- function(x) x

#' @rdname cell_limits
#' @examples
#' as.cell_limits("A1")
#' as.cell_limits("Q24")
#' as.cell_limits("A1:D8")
#' as.cell_limits("R5C11")
#' as.cell_limits("R2C3:R6C9")
#'
#' @export
as.cell_limits.character <- function(x) {

  stopifnot(length(x) == 1L)
  x_orig <- x

  x <- rm_dollar_signs(x)

  y <- unlist(strsplit(x, ":"))
  stopifnot(length(y) %in% 1:2)

  y <- rep_len(y[!grepl("\\s+", y)], 2)

  RC_regex <- "^R([0-9]+)C([0-9]+$)"
  A1_regex <- "^[A-Za-z]{1,3}[0-9]+$"

  if(all(grepl(A1_regex, y))) {
    y <- A1_to_RC(y)
  } else if(!all(grepl(RC_regex, y))) {
    stop("Trying to set cell limits, but requested range is invalid:\n",
      x_orig)
  }

  m <- regexec("^R([0-9]+)C([0-9]+$)", y)
  m2 <- regmatches(y, m)

  cell_limits(
    as.integer(vapply(m2, `[`, FUN.VALUE = character(1), 2)),
    as.integer(vapply(m2, `[`, FUN.VALUE = character(1), 3))
  )
}


#' Convert a cell_limits object to a cell range
#'
#' @param x a cell_limits object
#' @param RC logical, requesting "R1C1" positioning notation
#'
#' @return length one character vector holding a cell range, in either A1 or
#'   R1C1 positioning notation
#'
#' @examples
#' rgCL <- cell_limits(rows = c(1, 4), cols = c(1, 3))
#' as.range(rgCL)
#' as.range(rgCL, RC = TRUE)
#'
#' @export
as.range <- function(x, RC = FALSE) {

  stopifnot(inherits(x, "cell_limits"))

  if(any(is.na(unlist(x)))) return(NA_character_)

  range <- c(paste0("R", x$rows[1], "C", x$cols[1]),
             paste0("R", x$rows[2], "C", x$cols[2]))

  if(!RC) {
    range <- RC_to_A1(range)
  }

  paste(range, collapse = ":")
}
