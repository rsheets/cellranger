## proposal: have consistent cell specification interface with readxl
## therefore, isolating these functions and writing w/ zero pkg dependencies
## see https://github.com/hadley/readxl/issues/8

## read_excel() and presumably whatever wrappers I move to in googlesheets
## will call as.cell_limits on the input provided via range argument

cell_limits <- function(rows, cols) {

  NA_or_pos <- function(x) is.na(x) | x > 0
  stopifnot(is.numeric(rows), length(rows) == 2L, all(NA_or_pos(rows)))
  stopifnot(is.numeric(cols), length(cols) == 2L, all(NA_or_pos(rows)))

  if(!anyNA(rows)) stopifnot(rows[1] <= rows[2])
  if(!anyNA(cols)) stopifnot(cols[1] <= cols[2])

  structure(list(rows = rows, cols = cols), class = "cell_limits")

}

as.cell_limits <- function(x) UseMethod("as.cell_limits")
as.cell_limits.cell_limits <- function(x) x

#' Convert a cell range into a cell_limits object
#'
#' @param x character vector of length one, representing a cell range
#'
#' @return a \code{cell_limits} object, which is a list with components named
#'   \code{rows} and \code{cols}, each of the form \code{c(min, max}
#'
#' @examples
#' \dontrun{
#' as.cell_limits("A1")
#' as.cell_limits("Q24")
#' as.cell_limits("A1:D8")
#' as.cell_limits("R5C11")
#' as.cell_limits("R2C3:R6C9")
#' }
#'
#' @keywords internal
as.cell_limits.character <- function(x) {

  stopifnot(length(x) == 1L)
  x_orig <- x

  y <- unlist(strsplit(x, ":"))
  stopifnot(length(y) %in% 1:2)

  y <- rep_len(y[!grepl("\\s+", y)], 2)

  RC_regex <- "^R([0-9]+)C([0-9]+$)"
  A1_regex <- "^[A-Za-z]{1,3}[0-9]+$"

  if(all(grepl(A1_regex, y))) {
    y <- A1_to_RC(y)
  } else if(!all(grepl(RC_regex, y))) {
    mess <- sprintf(paste("Trying to set cell limits, but requested range is",
                          "invalid:\n %s\n"), x_orig)
    stop(mess)
  }

  m <- regexec("^R([0-9]+)C([0-9]+$)", y)
  m2 <- regmatches(y, m)

  cell_limits(
    as.integer(vapply(m2, `[`, FUN.VALUE = character(1), 2)),
    as.integer(vapply(m2, `[`, FUN.VALUE = character(1), 3))
  )

}

#' Convert A1 positioning notation to R1C1 notation
#'
#' @param x vector of cell positions in A1 notation
#'
#' @return vector of cell positions in R1C1 notation
#'
#' @examples
#' \dontrun{
#' A1_to_RC("A1")
#' A1_to_RC("AZ10")
#' A1_to_RC(c("A1", "AZ10"))
#' }
#'
#' @keywords internal
A1_to_RC <- function(x) {

  m <- regexec("[[:digit:]]*$", x)
  row_part <- as.integer(unlist(regmatches(x, m)))

  m <- regexec("^[[:alpha:]]*", x)
  col_part <- letter_to_num(unlist(regmatches(x, m)))

  paste0("R", row_part, "C", col_part)
}

#' Convert R1C1 positioning notation to A1 notation
#'
#' @param x vector of cell positions in R1C1 notation
#'
#' @return vector of cell positions in A1 notation
#'
#' @examples
#' \dontrun{
#' A1_to_RC("A1")
#' A1_to_RC("AZ10")
#' A1_to_RC(c("A1", "AZ10"))
#' }
#'
#' @keywords internal
RC_to_A1 <- function(x) {

  col_part <- sub("^R[0-9]+C([0-9]+)$", "\\1", x)
  col_part <- num_to_letter(as.integer(col_part))

  row_part <- sub("^R([0-9]+)C[0-9]+$", "\\1", x)

  paste0(col_part, row_part)
}


#' Convert column IDs from letter representation to integer
#'
#' @param x character vector of letter-style column IDs (case insensitive)
#'
#' @return vector of positive integers identifying columns numerically
#'
#' @examples
#' \dontrun{
#' letter_to_num('Z')
#' letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ'))
#' }
#'
#' @keywords internal
letter_to_num <- function(x) {

  x <- strsplit(toupper(x), '')
  x <- lapply(x, match, table = LETTERS)
  x <- lapply(x, function(z) sum((26 ^ rev(seq_along(z) - 1)) * z))
  unlist(as.integer(x))

}

#' Convert column IDs from numeric to letter representation
#'
#' @param x vector of numeric column IDs
#'
#' @return character vector of letter-style column IDs
#'
#' @examples
#' \dontrun{
#' num_to_letter(28)
#' num_to_letter(900)
#' num_to_letter(18278)
#' num_to_letter(c(25, 52, 900, 18278))
#' }
#'
#' @keywords internal
num_to_letter <- function(y) {
  # FYI Google spreadsheets have 300 columns max
  # Excel 2010 spreadsheets have up to 16,384 columns
  #  ZZ <-->    702
  # ZZZ <--> 18,278

  # fcn to express column number in this weird form of base 26
  jfun <- function(div) {
    ret <- integer()
    while(div > 0) {
      remainder <- ((div - 1) %% 26) + 1
      ret <- c(remainder, ret)
      div <- (div - remainder) %/% 26
    }
    ret
  }

  y <- lapply(y, jfun)
  y <- lapply(y, function(x) LETTERS[x])
  y <- lapply(y, paste, collapse = '')
  unlist(y)

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
#' \dontrun{
#' rgCL <-
#'   structure(list(rows = c(1, 4), cols = c(1, 3)), class = "cell_limits")
#' convert_cell_limits_to_range(rgCL)
#' convert_cell_limits_to_range(rgCL, RC = TRUE)
#' }
#'
#' @keywords internal
convert_cell_limits_to_range <- function(x, RC = FALSE) {

  stopifnot(inherits(x, "cell_limits"))

  range <- c(paste0("R", x$rows[1], "C", x$cols[1]),
             paste0("R", x$rows[2], "C", x$cols[2]))

  if(!RC) {
    range <- RC_to_A1(range)
  }

  paste(range, collapse = ":")
}
