#' Specify cell limits via an anchor cell
#'
#' Specify the targetted cell rectangle via an "upper left" anchor cell and the
#' rectangle's row and column extent. The extent can be specified directly via
#' \code{dims} or indirectly via the \code{input} object. Specification via
#' \code{input} anticipates a write operation into the spreadsheet. If
#' \code{input} is one-dimensional, the \code{byrow} argument controls whether
#' the rectangle will extend down from the anchor or to the right. If
#' \code{input} is two-dimensional, the \code{header} argument controls whether
#' cells will be reserved for column or variable names.
#'
#' @param anchor character, specifying the upper left cell in "A1" or "R1C1"
#'   notation
#' @param dim integer vector, of length two, holding the number of rows and
#'   columns of the targetted rectangle
#' @param input a one- or two-dimensioanl input object, used to determine the
#'   extent of the targetted rectangle
#' @param header logical, indicating whether a row should be reserved for the
#'   column or variable names of a two-dimensional input
#' @param byrow logical, indicating whether a one-dimensional input should run
#'   down or to the right
#'
#' @return a \code{cell_limits} object
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
#' dim(anchored(anchor = "R4C2", dim = c(8, 2)))
#'
#' (input <- head(iris))
#' anchored(input = input)
#' as.range(anchored(input = input))
#' dim(anchored(input = input))
#'
#' anchored(input = input, header = FALSE)
#' as.range(anchored(input = input, header = FALSE))
#' dim(anchored(input = input, header = FALSE))
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
                     header = TRUE, byrow = FALSE) {

  anchorCL <- as.cell_limits(anchor)
  stopifnot(dim(anchorCL) == c(1L, 1L))

  if(is.null(input)) {

    input_extent <- as.integer(dim)

  } else {

    if(is.null(dim(input))) { # input is 1-dimensional

      input_extent <- c(length(input), 1L)
      if(byrow) {
        input_extent <- rev(input_extent)
      }

    } else {                  # input is 2-dimensional

      stopifnot(length(dim(input)) == 2L)
      input_extent <- dim(input)
      if(header) {
        input_extent[1] <- input_extent[1] + 1
      }

    }

  }

  cell_limits(rows = anchorCL$rows + c(0L, input_extent[1] - 1),
              cols = anchorCL$cols + c(0L, input_extent[2] - 1))

}

