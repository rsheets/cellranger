## vectorized over x and always returns list
## strict = FALSE --> pure relative references treated as absolute
## example: F4 treated like $F$4
A1_to_ra_ref <- function(x, strict = TRUE) {
  y <- rematch::re_match(.cr$A1_ncg_rx, x)
  ## y is character matrix, rows are elements of x, cols are pieces of the ref
  ## input D$56 gives this row
  ## row_abs = "$" row_ref = "56" col_abs = "" col_ref = "D"
  ## presence of "$" decoration in original row or col ref --> absolute
  if (!strict) { # absolutize pure relative refs; mixed refs not changed
    rel <- y[ , "col_abs"] == "" & y[ , "row_abs"] == ""
    y[rel, c("col_abs", "row_abs")] <- "$"
  }
  row_not_abs <- y[ , "row_abs"] != "$"
  y[row_not_abs, "row_ref"] <- NA
  col_not_abs <- y[ , "col_abs"] != "$"
  y[col_not_abs, "col_ref"] <- NA
  mapply(ra_ref,
         stats::setNames(y[ , "row_ref"], y[ , ".match"]), y[ , "row_abs"] == "$",
         letter_to_num(y[ , "col_ref"]),                 y[ , "col_abs"] == "$",
         SIMPLIFY = FALSE)
}

## vectorized over x and always returns list
R1C1_to_ra_ref <- function(x) {
  y <- rematch::re_match(.cr$R1C1_ncg_rx, x)
  ## y is character matrix, rows are elements of x, cols are pieces of the ref
  ## input R[-4]C7 gives this row
  ## row_abs = "[" row_ref = "-4" col_abs = "" col_ref = "7"
  ## presence of square brackets `[x]` --> relative
  ## EXCEPT when row or column reference is empty, e.g., RC, RCx, RxC
  ## which means "this row or column" --> offset is 0 and ref is relative
  row_ref_missing <- y[ , "row_ref"] == ""
  y[row_ref_missing, "row_abs"] <- "["
  y[row_ref_missing, "row_ref"] <- "0"
  col_ref_missing <- y[ , "col_ref"] == ""
  y[col_ref_missing, "col_abs"] <- "["
  y[col_ref_missing, "col_ref"] <- "0"
  mapply(ra_ref,
         stats::setNames(y[ , "row_ref"], y[ , ".match"]), y[ , "row_abs"] == "",
         y[ , "col_ref"],                  y[ , "col_abs"] == "",
         SIMPLIFY = FALSE)
}

#' Convert cell reference strings from A1 to R1C1 format
#'
#' Convert cell reference strings from A1 to R1C1 format. Strictly speaking,
#' this only makes sense for absolute references, such as \code{"$B$4"}. Why?
#' Because otherwise, we'd have to know the host cell of the reference. Set
#' \code{strict = FALSE} to relax and treat pure relative references, like
#' (\code{"B4"}), as if they are absolute. Mixed references, like
#' (\code{"B$4"}), will always return \code{NA}, no matter the value of
#' \code{strict}.
#'
#' @param x character vector of cell references in A1 format
#' @template param-strict
#'
#' @return character vector of absolute cell references in R1C1 format
#'
#' @examples
#' A1_to_R1C1("$A$1")
#' A1_to_R1C1("A1")                 ## raises a warning, returns NA
#' A1_to_R1C1("A1", strict = FALSE) ## unless strict = FALSE
#' A1_to_R1C1(c("A1", "B$4")) ## raises a warning, includes an NA, because
#' A1_to_R1C1(c("A1", "B$4"), strict = FALSE) ## mixed ref always returns NA
#' @export
A1_to_R1C1 <- function(x, strict = TRUE) {
  stopifnot(is.character(x), all(is_A1(x)))
  y <- unname(A1_to_ra_ref(x, strict = strict))
  not_abs <- vapply(y, is_not_abs_ref, logical(1))
  if (any(not_abs)) {
    warning("Mixed or relative cell references found ... NAs generated",
            call. = FALSE)
  }
  vapply(y, to_string, character(1))
}

#' Convert R1C1 positioning notation to A1 notation
#'
#' Convert cell reference strings from R1C1 to A1 format. This only makes sense
#' for absolute references, such as \code{"R4C2"}. Why? Because otherwise, we'd
#' have to know the host cell of the reference. Relative and mixed references,
#' like (\code{"R[3]C[-1]"} and \code{"R[1]C5"}), will therefore return
#' \code{NA}.
#'
#' @param x vector of cell positions in R1C1 notation
#' @template param-strict
#'
#' @return character vector of absolute cell references in A1 notation
#'
#' @examples
#' R1C1_to_A1("R1C1")
#' R1C1_to_A1("R10C52", strict = FALSE)
#' R1C1_to_A1(c("R1C1", "R10C52", "RC4", "R[-3]C[9]"))
#' @export
R1C1_to_A1 <- function(x, strict = TRUE) {
  stopifnot(is.character(x), all(is_R1C1(x)))
  y <- unname(R1C1_to_ra_ref(x))
  abs <- vapply(y, is_abs_ref, logical(1))
  if (any(!abs)) {
    warning("Ambiguous cell references ... NAs generated", call. = FALSE)
    y[!abs] <- lapply(y[!abs], function(x) {
      ra_ref(row_ref = NA, row_abs = NA, col_ref = NA, col_abs = NA,
             sheet = x$sheet, file = x$file)
    })
  }
  vapply(y, to_string, character(1), fo = "A1", strict = strict)
}
