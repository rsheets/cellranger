#' Convert column IDs from letter representation to integer
#'
#' @param x character vector of letter-style column IDs (case insensitive)
#'
#' @return vector of positive integers, giving associated column numbers
#'
#' @examples
#' letter_to_num('Z')
#' letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ'))
#' letter_to_num(c(NA, ''))
#'
#' @export
letter_to_num <- function(x) {

  stopifnot(is.character(x))

  x <- strsplit(toupper(x), '')
  x <- lapply(x, char0_to_NA)
  x <- lapply(x, match, table = LETTERS)
  x <- lapply(x, function(z) sum((26 ^ rev(seq_along(z) - 1)) * z))
  as.integer(x)

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
#' num_to_letter(c(NA, 0, 4.8, -4))
#'
#' @export
num_to_letter <- function(y) {

  stopifnot(is.numeric(y))

  # FYI Google spreadsheets have 300 columns max
  # Excel 2010 spreadsheets have up to 16,384 columns
  #  ZZ <-->    702
  # ZZZ <--> 18,278

  # fcn to express column number in this weird form of base 26
  jfun <- function(div) {
    if (is.na(div)) {
      return(NA_character_)
    }

    ret <- integer()
    while(div > 0) {
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
