## nqa = "no questions asked!"
## let calling functions worry about whether this conversion is a good idea
A1_to_ra_ref_nqa <- function(x) {
  y <- extract_named_captures(x, .cr$A1_ncg_rx)
  ## example: D$56 comes back as named list
  ## rowAbs = "$" rowRef = "56" colAbs = "" colRef = "D"
  ## presence of "$" decoration in original row or col ref --> absolute
  ra_ref(rowRef = as.integer(y$rowRef), rowAbs = nzchar(y$rowAbs),
         colRef = letter_to_num(y$colRef), colAbs = nzchar(y$colAbs))
}

## this IS vectorized over x and always returns list
A1_to_ra_ref <- function(x, strict = TRUE) {
  y <- lapply(x, A1_to_ra_ref_nqa)
  abs <- vapply(y, is_abs_ref, logical(1))
  if (strict) {
    if (any(!abs)) {
      warning("Relative and/or mixed cell references ... NAs generated",
              call. = FALSE)
      y[!abs] <- NA
    }
  } else {
    y <- lapply(y, absolutize)
  }
  y
}

R1C1_to_ra_ref <- function(x) {
  y <- extract_named_captures(x, .cr$R1C1_ncg_rx)
  ## example: R[-4]C7 comes back as named list
  ## rowAbs = "[" rowRef = "-4" colAbs = "" colRef = "7"
  ## presence of square brackets `[x]` --> relative
  y$rowAbs <- !nzchar(y$rowAbs)
  y$colAbs <- !nzchar(y$colAbs)
  ## EXCEPT when row or column reference is empty, e.g., RC, RCx, RxC
  ## which means "this row or column" --> offset is 0 and ref is relative
  if (y$rowRef == "") {
    y$rowAbs <- FALSE
    y$rowRef <- 0
  }
  if (y$colRef == "") {
    y$colAbs <- FALSE
    y$colRef <- 0
  }
  ra_ref(rowRef = as.integer(y$rowRef), rowAbs = y$rowAbs,
         colRef = as.integer(y$colRef), colAbs = y$colAbs)
}

#' Convert cell reference strings from A1 to R1C1 format
#'
#' Convert cell reference strings from A1 to R1C1 format. Strictly speaking,
#' this only makes sense for absolute references, such as \code{"$B$4"}. Why?
#' Because otherwise, we'd have to know the host cell of the reference. Set
#' \code{strict = FALSE} to relax and treat relative (\code{"B4"}) and mixed
#' (\code{"B$4"}) references as if they are absolute.
#'
#' @param x character vector of cell references in A1 format
#' @param strict logical, indicates that only absolute references should be
#'   converted; defaults to \code{TRUE}
#'
#' @return character vector of absolute cell references in R1C1 format
#'
#' @examples
#' A1_to_RC("$A$1")
#' A1_to_RC("A1")              ## raises a warning, returns NA
#' A1_to_RC("A1", strict = FALSE)
#' A1_to_RC(c("$A$1", "AZ10")) ## raises a warning, includes an NA
#' A1_to_RC(c("$A$1", "AZ10"), strict = FALSE)
#' \dontrun{
#' A1_to_RC("Q0", strict = FALSE) ## error because there is no row 0
#' }
#' @export
A1_to_RC <- function(x, strict = TRUE) {
  stopifnot(is.character(x), all(grepl(.cr$is_A1_rx, x)))
  y <- A1_to_ra_ref(x, strict = strict)
  vapply(y,
         function(z) {
           if (inherits(z, "ra_ref")) to_string(z) else NA_character_
           },
         character(1))
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
