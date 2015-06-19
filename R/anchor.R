#' Specify cell limits via an anchor cell
#'
#' Specify the targetted cell rectangle via an upper left anchor cell and the
#' rectangle's row and column extent. The extent can be specified directly via
#' \code{dims} or indirectly via the \code{input} object. Specification via
#' \code{input} anticipates a write operation into the spreadsheet. If
#' \code{input} is one-dimensional, the \code{byrow} argument controls whether
#' the rectangle will extend down from the anchor or to the right. If
#' \code{input} is two-dimensional, the \code{col_names} argument controls
#' whether cells will be reserved for column or variable names. If
#' \code{col_names} is unspecified, default behavior is to set it to \code{TRUE}
#' if \code{input} has columns names and \code{FALSE} otherwise.
#'
#' @param anchor character, specifying the upper left cell in "A1" or "R1C1"
#'   notation
#' @param dim integer vector, of length two, holding the number of rows and
#'   columns of the targetted rectangle; ignored if \code{input} is provided
#' @param input a one- or two-dimensioanl input object, used to determine the
#'   extent of the targetted rectangle
#' @param col_names logical, indicating whether a row should be reserved for the
#'   column or variable names of a two-dimensional input; if omitted, will be
#'   determined by checking whether \code{input} has column names
#' @param byrow logical, indicating whether a one-dimensional input should run
#'   down or to the right
#'
#' @return a \code{\link{cell_limits}} object
#'
#' @examples
#' anchored()
#' as.range(anchored())
#' dim(anchored())
#'
#' anchored("Q24")
#' as.range(anchored("Q24"))
#' dim(anchored("Q24"))
#'
#' anchored(anchor = "R4C2", dim = c(8, 2))
#' as.range(anchored(anchor = "R4C2", dim = c(8, 2)))
#' as.range(anchored(anchor = "R4C2", dim = c(8, 2)), RC = TRUE)
#' dim(anchored(anchor = "R4C2", dim = c(8, 2)))
#'
#' (input <- head(iris))
#' anchored(input = input)
#' as.range(anchored(input = input))
#' dim(anchored(input = input))
#'
#' anchored(input = input, col_names = FALSE)
#' as.range(anchored(input = input, col_names = FALSE))
#' dim(anchored(input = input, col_names = FALSE))
#'
#' (input <- LETTERS[1:8])
#' anchored(input = input)
#' as.range(anchored(input = input))
#' dim(anchored(input = input))
#'
#' anchored(input = input, byrow = TRUE)
#' as.range(anchored(input = input, byrow = TRUE), RC = TRUE)
#' dim(anchored(input = input, byrow = TRUE))
#'
#' @export
anchored <- function(anchor = "A1",
                     dim = c(1L, 1L), input = NULL,
                     col_names = NULL, byrow = FALSE) {

  anchorCL <- as.cell_limits(anchor)
  stopifnot(dim(anchorCL) == c(1L, 1L), isTOGGLE(col_names), isTOGGLE(byrow))

  if(is.null(input)) {

    stopifnot(length(dim) == 2L)
    input_extent <- as.integer(dim)
    if(is.null(col_names)) {
      col_names <- FALSE
    }

  } else {

    if(is.null(dim(input))) { # input is 1-dimensional

      col_names <- FALSE
      input_extent <- c(length(input), 1L)
      if(byrow) {
        input_extent <- rev(input_extent)
      }

    } else {                  # input is 2-dimensional

      stopifnot(length(dim(input)) == 2L)
      if(is.null(col_names)) {
        col_names <- !is.null(colnames(input))
      }
      input_extent <- dim(input)

    }

  }

  if(col_names) {
    input_extent[1] <- input_extent[1] + 1
  }

  cell_limits(ul = anchorCL$ul,
              lr = anchorCL$lr + input_extent - 1)

}

