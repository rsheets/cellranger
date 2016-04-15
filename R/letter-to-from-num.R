#' Convert between letter and integer representations of column IDs
#'
#' Convert "A1"-style column IDs from a letter representation to an integer,
#' e.g. column A becomes 1, column D becomes 4, etc. Or go the other way around.
#'
#' \itemize{
#' \item Google Sheets have up to 300 columns (column KN).
#' \item Excel 2010 spreadsheets have up to 16,384 columns (column XFD).
#' \item ZZ is column 702.
#' \item ZZZ is column 18,278 (no known spreadsheet actually goes that high).
#' }
#' @name letter-num-conversion
#'
#' @param x a character vector of "A1" style column IDs (case insensitive)
#' @param y a vector of integer column IDs
#' @return a vector of column IDs, either character or integer
NULL

#' @rdname letter-num-conversion
#' @examples
#' letter_to_num('Z')
#' letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ'))
#' letter_to_num(c(NA, ''))
#' @export
letter_to_num <- function(x) {

  stopifnot(is.character(x))

  x <- strsplit(toupper(x), '')
  x <- lapply(x, char0_to_NA)
  x <- lapply(x, match, table = LETTERS)
  x <- lapply(x, function(z) sum((26 ^ rev(seq_along(z) - 1)) * z))
  as.integer(x)

}

#' @rdname letter-num-conversion
#' @examples
#' num_to_letter(28)
#' num_to_letter(900)
#' num_to_letter(18278)
#' num_to_letter(c(25, 52, 900, 18278))
#' num_to_letter(c(NA, 0, 4.8, -4))
#' @export
num_to_letter <- function(y) {

  stopifnot(is.numeric(y))

  # fcn to express column number in this weird form of base 26
  jfun <- function(div) {
    if (is.na(div)) {
      return(NA_character_)
    }

    ret <- integer()
    while (div > 0) {
      remainder <- ((div - 1) %% 26) + 1
      ret <- c(remainder, ret)
      div <- (div - remainder) %/% 26
    }
    paste(LETTERS[ret], collapse = "")
  }

  ret <- vapply(y, jfun, character(1))

  ## 0 becomes "", so we set that to NA here
  ifelse(ret == "", NA_character_, ret)
}
