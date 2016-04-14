#' Convert A1 positioning notation to R1C1 notation
#'
#' Convert "A1" style cell references to "R1C1" style. Dollar signs will be
#' ignored.
#'
#' @param x vector of cell positions in A1 notation
#'
#' @return vector of cell positions in R1C1 notation
#'
#' @examples
#' A1_to_RC("A1")
#' A1_to_RC("AZ10")
#' A1_to_RC("AZ$10")
#' A1_to_RC(c("A1", "AZ10"))
#' A1_to_RC(c("", NA, "Q0"))
#'
#' @export
A1_to_RC <- function(x) {

  stopifnot(is.character(x))

  x <- rm_dollar_signs(x)

  m <- regexec("[[:digit:]]*$", x)
  m <- regmatches(x, m)
  row_part <- as.integer(vapply(m, char0_to_NA, character(1)))
  row_part <- ifelse(row_part > 0, row_part, NA)

  m <- regexec("^[[:alpha:]]*", x)
  m <- regmatches(x, m)
  col_part <- letter_to_num(vapply(m, char0_to_NA, character(1)))

  ifelse(is.na(row_part) | is.na(col_part), NA,
         paste0("R", row_part, "C", col_part))
}

#' Convert R1C1 positioning notation to A1 notation
#'
#' @param x vector of cell positions in R1C1 notation
#'
#' @return vector of cell positions in A1 notation
#'
#' @examples
#' RC_to_A1("R1C1")
#' RC_to_A1("R10C52")
#' RC_to_A1(c("R1C1", "R10C52"))
#' RC_to_A1(c("", NA, "R0C0"))
#'
#' @export
RC_to_A1 <- function(x) {

  stopifnot(is.character(x))

  col_part <- sub("^R[0-9]+C([0-9]+)$", "\\1", x)
  col_part <- num_to_letter(as.integer(col_part))

  row_part <- sub("^R([0-9]+)C[0-9]+$", "\\1", x)
  row_part <- as.integer(row_part)
  row_part <- ifelse(row_part > 0, row_part, NA)

  ifelse(is.na(row_part) | is.na(col_part), NA,
         paste0(col_part, row_part))
}
