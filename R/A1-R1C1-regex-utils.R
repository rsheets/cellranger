.cr <- new.env(parent = emptyenv())

## for validating single cell references
.cr$A1_rx <- "^\\$?[A-Za-z]{1,3}\\$?[0-9]{1,5}$"
.cr$R1C1_rx <- "^R\\[?\\-?[0-9]*\\]?C\\[?\\-?[0-9]*\\]?$"

#' Test cell reference strings
#'
#' Test cell reference strings for a specific format.
#'
#' @param x character vector of cell reference strings
#'
#' @return a logical vector
#' @name is_A1
#' @examples
#' is_A1("A1")
#' is_R1C1("A1")
#' is_R1C1("R4C12")
#'
#' x <- c("A1", "$A4", "$b$12", "RC1", "R[-4]C9", "R5C3")
#' data.frame(x, is_A1(x), is_R1C1(x))
NULL

#' @describeIn is_A1 A1 format, case insenstive; relative, absolute, or mixed
#' @export
is_A1 <- function(x) grepl(.cr$A1_rx, x)

#' @describeIn is_A1 R1C1 format; relative, absolute, or mixed
#' @export
is_R1C1 <- function(x) grepl(.cr$R1C1_rx, x)

#' Guess cell reference string format
#'
#' Guess if cell references are in R1C1 or A1 format.
#'
#' @param x character vector of cell reference strings
#' @param fo default to assume if format is ambiguous
#'
#' @return character vector consisting of \code{R1C1}, \code{A1}, or \code{NA}
#' @export
#'
#' @examples
#' A1 <- c("A1", "$A1", "A$1", "$A$1", "a1")
#' guess_fo(A1)
#' R1C1 <- c("R1C1", "R1C[-1]", "R[-1]C1", "R[-1]C[9]")
#' guess_fo(R1C1)
#'
#' guess_fo("RC2")
#' guess_fo("12")
#' guess_fo(12)
guess_fo <- function(x, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  is_R1C1 <- is_R1C1(x)
  is_A1 <- is_A1(x)
  out <- ifelse(is_R1C1, "R1C1", ifelse(is_A1, "A1", NA_character_))
  both <- is_R1C1 & is_A1
  neither <- is.na(out)
  if (any(both)) {
    out[both] <- fo
    ## OMFG this can actually happen. Example: RCx
    warning("Not clear if cell reference is in A1 or R1C1 format. Example:\n",
            x[both][1], "\nDefaulting to ", fo,
            call. = FALSE)
  }
  if (any(neither)) {
    warning("Cell reference follows neither the A1 nor R1C1 format. Example:\n",
            x[neither][1], "\nNAs generated.",
            call. = FALSE)
  }
  out
}

## for parsing single cell references
.cr$A1_ncg_rx <-
  paste0("(?P<col_abs>\\$?)(?P<col_ref>[A-Za-z]{1,3})",
         "(?P<row_abs>\\$?)(?P<row_ref>[0-9]+)")
.cr$R1C1_ncg_rx <-
  paste0("^R(?P<row_abs>\\[?)(?P<row_ref>[0-9\\-]*)(?:\\]?)",
         "C(?P<col_abs>\\[?)(?P<col_ref>[0-9\\-]*)(?:\\]?)$")

## for parsing cell (area) references that are possibly qualified by
## file and/or worksheet name
.cr$filename_rx = "(?:^\\[([^\\]]+)\\])?"
.cr$worksheetname_rx <- "(?:'?([^']+)'?!)?"
.cr$ref_rx <- "([a-zA-Z0-9:\\-$\\[\\]]+)"
.cr$string_rx <- sprintf("^(?:%s%s%s|(.*))$", .cr$filename_rx,
                         .cr$worksheetname_rx, .cr$ref_rx)

parse_ref_string <- function(x, fo = NULL) {
  parsed <- as.list(rematch::re_match(.cr$string_rx, x)[1, , drop = TRUE])
  names(parsed) <- c("input", "file", "sheet", "ref", "invalid")
  parsed$ref_v <- unlist(strsplit(parsed$ref, ":"))
  stopifnot(length(parsed$ref_v) %in% 1:2)
  if (is.null(fo)) {
    fo_v <- guess_fo(parsed$ref_v)
    parsed$fo <- unique(fo_v)
    if (length(parsed$fo) > 1) {
      stop("Cell references aren't uniformly A1 or R1C1 format:\n",
           parsed$ref, call. = FALSE)
    }
  } else {
    parsed$fo <- match.arg(fo, c("R1C1", "A1"))
  }
  parsed
}
