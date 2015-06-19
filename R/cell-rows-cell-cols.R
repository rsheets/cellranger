#' Specify cell limits only for rows
#'
#' How does this differ from \code{\link{cell_limits}}? Here the input can have
#' length greater than 2, i.e. the rows can be specified as \code{1:n}. If the
#' length is greater than 2, both the min and max are taken with \code{NA.rm =
#' TRUE}. Note it is not possible to request non-contiguous rows, i.e. rows 1,
#' 2, and 5. In this case, the requested rows will run from the minimum of 1 to
#' the maximum of 5.
#'
#' @param x numeric vector of row limits; if length greater than two, min and
#'   max will be taken with \code{NA.rm = TRUE}
#'
#' @return a \code{\link{cell_limits}} object
#'
#' @examples
#' cell_rows(c(NA, 3))
#' cell_rows(c(7, NA))
#' cell_rows(4:16)
#' cell_rows(c(3, NA, 10))
#'
#' dim(cell_rows(1:5))
#'
#' @export
cell_rows <- function(x) {

  if(all(is.na(x))) {
    return(cell_limits())
  }

  stopifnot(is.numeric(x))

  if (length(x) != 2L) {
    x <- range(x, na.rm = TRUE)
  }

  cell_limits(as.integer(c(x[1], NA)), as.integer(c(x[2], NA)))
}

#' Specify cell limits only for columns
#'
#' How does this differ from \code{\link{cell_limits}}? Two ways. First, the
#' input can have length greater than 2, i.e. the columns can be specified as
#' \code{1:n}. If the length is greater than 2, both the min and max are taken
#' with \code{NA.rm = TRUE}. Note it is not possible to request non-contiguous
#' columns, i.e. columns 1, 2, and 5. In this case, the requested columns will
#' run from the minimum of 1 to the maximum of 5. Second, the input can be given
#' in the letter-based format spreadsheets use to label columns.
#'
#' @param x vector of column limits; if character, converted to numeric; if
#'   length greater than two, min and max will be taken with \code{NA.rm = TRUE}
#'
#' @return a \code{\link{cell_limits}} object
#'
#' @examples
#' cell_cols(c(NA, 3))
#' cell_cols(c(7, NA))
#' cell_cols(4:16)
#' cell_cols(c(3, NA, 10))
#'
#' cell_cols("C:G")
#' cell_cols(c("B", NA))
#' cell_cols(LETTERS)
#'
#' @export
cell_cols <- function(x) {

  if(all(is.na(x))) {
    return(cell_limits())
  }

  stopifnot(is.numeric(x) || is.character(x))

  if(is.character(x)) {

    if(length(x) == 1L) {
      x <- strsplit(x, ":")[[1]]
    }

    x <- letter_to_num(x)
  }

  if (length(x) != 2L) {
    x <- range(x, na.rm = TRUE)
  }

  cell_limits(as.integer(c(NA, x[1])), as.integer(c(NA, x[2])))
}

