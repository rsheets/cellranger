#' Convert column IDs from letter representation to integer
#'
#' @param x character vector of letter-style column IDs (case insensitive)
#'
#' @return vector of positive integers, giving associated column numbers
#'
#' @examples
#' letter_to_num('Z')
#' letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ'))
#'
#' @export
letter_to_num <- function(x) {

  x <- strsplit(toupper(x), '')
  x <- lapply(x, match, table = LETTERS)
  x <- lapply(x, function(z) sum((26 ^ rev(seq_along(z) - 1)) * z))
  unlist(as.integer(x))

}

#' Convert column numbers to letter representation
#'
#' @param y vector of column numbers
#'
#' @return character vector of letter-style column IDs
#'
#' @examples
#' num_to_letter(28)
#' num_to_letter(900)
#' num_to_letter(18278)
#' num_to_letter(c(25, 52, 900, 18278))
#'
#' @export
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

#' Convert A1 positioning notation to R1C1 notation
#'
#' @param x vector of cell positions in A1 notation
#'
#' @return vector of cell positions in R1C1 notation
#'
#' @examples
#' A1_to_RC("A1")
#' A1_to_RC("AZ10")
#' A1_to_RC(c("A1", "AZ10"))
#'
#' @export
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
#' RC_to_A1("R1C1")
#' RC_to_A1("R10C52")
#' RC_to_A1(c("R1C1", "R10C52"))
#'
#' @export
RC_to_A1 <- function(x) {

  col_part <- sub("^R[0-9]+C([0-9]+)$", "\\1", x)
  col_part <- num_to_letter(as.integer(col_part))

  row_part <- sub("^R([0-9]+)C[0-9]+$", "\\1", x)

  paste0(col_part, row_part)
}
