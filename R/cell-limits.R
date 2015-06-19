#' Create a cell_limits object
#'
#' The expectation is that \code{\link{as.cell_limits}} will be called to
#' process user-provided input specifying the target cell range for read/write
#' operations on a spreadsheet. Downstream code can be written assuming cell
#' limits are stored in a valid \code{cell_limits} object.
#'
#' A \code{cell_limits} object is a list with two components:
#'
#' \itemize{
#'   \item \code{ul} vector specifying upper left cell of target rectangle, of
#'   the form \code{c(ROW_MIN, COL_MIN)}
#'   \item \code{lr} vector specifying lower right cell of target rectangle, of
#'   the form \code{c(ROW_MAX, COL_MAX)}
#' }
#'
#' This follows the spreadsheet convention where a cell range is described as
#' \code{UPPER_LEFT_CELL:LOWER_RIGHT_CELL}. For rows and columns, the associated
#' \code{MIN} and \code{MAX} are positive integers, where the minimum should be
#' less than or equal to the maximum. A value of \code{NA} means the
#' corresponding limit is left unspecified. Therefore a verbose way to specify
#' no limits at all would be \code{cell_limits(c(NA, NA), c(NA, NA))}. If the
#' maximum row or column is specified but the associated minimum is not, then
#' the minimum it is set to 1.
#'
#' When specified via character, spreadsheet ranges can be given in "A1"
#' notation or "R1C1" notation and dollar signs will be ignored, i.e.
#' "A$1:$B$32" is equivalent to "A1:B32".
#'
#' @param ul vector identifying upper left cell of target rectangle
#' @param lr vector identifying lower right cell of target rectangle
#' @param x input to convert into a \code{cell_limits} object
#'
#' @return a \code{cell_limits} object
#'
#' @examples
#' cell_limits(c(1, 3), c(1, 5))
#' cell_limits(c(NA, 7), c(3, NA))
#' cell_limits(c(NA, 7))
#' cell_limits(lr = c(3, 7))
#'
#' dim(as.cell_limits("A1:F10"))
#'
#' @export
cell_limits <- function(ul = c(NA_integer_, NA_integer_),
                        lr = c(NA_integer_, NA_integer_)) {

  stopifnot(length(ul) == 2L, length(lr) == 2L)

  ul <- as.integer(ul)
  lr <- as.integer(lr)

  NA_or_pos <- function(x) is.na(x) | x > 0
  stopifnot(all(NA_or_pos(ul)))
  stopifnot(all(NA_or_pos(lr)))

  if(is.na(ul[1]) && !is.na(lr[1])) ul[1] <- 1L
  if(is.na(ul[2]) && !is.na(lr[2])) ul[2] <- 1L

  rows <- c(ul[1], lr[1])
  cols <- c(ul[2], lr[2])

  if(!anyNA(rows)) stopifnot(rows[1] <= rows[2])
  if(!anyNA(cols)) stopifnot(cols[1] <= cols[2])

  structure(list(ul = ul, lr = lr),
            class = c("cell_limits", "list"))

}

#' @export
print.cell_limits <- function(x, ...) {
  ul <- ifelse(is.na(x$ul), "-", as.character(x$ul))
  lr <- ifelse(is.na(x$lr), "-", as.character(x$lr))

  cat("<cell_limits (", ul[1], ", ", ul[2], ") x (",
      lr[1], ", ", lr[2], ")>\n",
      sep = "")
}

#' @rdname cell_limits
#' @export
dim.cell_limits <- function(x) c(x$lr[1] - x$ul[1], x$lr[2] - x$ul[2]) + 1

#' @rdname cell_limits
#' @export
as.cell_limits <- function(x) UseMethod("as.cell_limits")

#' @rdname cell_limits
#' @export
as.cell_limits.cell_limits <- function(x) x

#' @rdname cell_limits
#' @export
as.cell_limits.NULL <- function(x) cell_limits()

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

  jfun <- function(x) as.integer(x[2:3])
  cell_limits(
    jfun(m2[[1]]),
    jfun(m2[[2]])
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
#' rgCL <- cell_limits(ul = c(1, 2), lr = c(7, 6))
#' as.range(rgCL)
#' as.range(rgCL, RC = TRUE)
#'
#' @export
as.range <- function(x, RC = FALSE) {

  stopifnot(inherits(x, "cell_limits"))

  if(any(is.na(unlist(x)))) return(NA_character_)

  range <- c(paste0("R", x$ul[1], "C", x$ul[2]),
             paste0("R", x$lr[1], "C", x$lr[2]))

  if(!RC) {
    range <- RC_to_A1(range)
  }

  paste(range, collapse = ":")
}
