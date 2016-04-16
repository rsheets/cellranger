A1_to_ra_ref_ONE <- function(x) {
  ## WARNING: operates on a "no questions asked!" basis
  ## this conversion arguably makes no sense for
  ## relative references like A1 and, especially,
  ## mixed references like A$1 or $A1
  ## RELATIVE TO WHAT?!?
  ## but, anyway, the calling function is responsible for that
  y <- extract_named_captures(x, .cr$A1_ncg_rx)
  ## example: D$56 comes back as named list
  ## rowAbs = "$" rowRef = "56" colAbs = "" colRef = "D"
  ## presence of "$" decoration in original row or col ref --> absolute
  ra_ref(rowRef = as.integer(y$rowRef), rowAbs = nzchar(y$rowAbs),
         colRef = letter_to_num(y$colRef), colAbs = nzchar(y$colAbs))
}

## vectorized over x and always returns list
A1_to_ra_ref <- function(x, strict = TRUE) {
  y <- lapply(x, A1_to_ra_ref_ONE)
  rel <- vapply(y, is_rel_ref, logical(1))
  mixed <- vapply(y, is_mixed_ref, logical(1))
  if (strict && any(rel)) mixed <- rel | mixed
  if (any(mixed)) {
    warning("Ambiguous cell references ... NAs generated", call. = FALSE)
    ## TO DO: maybe I need to make a ra_ref with NAs everywhere instead?
    y[mixed] <- NA
  }
  if (!strict && any(rel)) y[rel] <- lapply(y[rel], absolutize)
  y
}

R1C1_to_ra_ref_ONE <- function(x) {
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

## vectorized over x and always returns list
R1C1_to_ra_ref <- function(x) lapply(x, R1C1_to_ra_ref_ONE)

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
#' @param strict logical, indicates that only absolute references should be
#'   converted; defaults to \code{TRUE}
#'
#' @return character vector of absolute cell references in R1C1 format
#'
#' @examples
#' A1_to_RC("$A$1")
#' A1_to_RC("A1")                 ## raises a warning, returns NA
#' A1_to_RC("A1", strict = FALSE) ## unless strict = FALSE
#' A1_to_RC(c("$A$1", "B$4")) ## raises a warning, includes an NA
#' A1_to_RC(c("$A$1", "B$4"), strict = FALSE) ## here too
#' @export
A1_to_RC <- function(x, strict = TRUE) {
  stopifnot(is.character(x), all(grepl(.cr$is_A1_rx, x)))
  y <- A1_to_ra_ref(x, strict = strict)
  vapply(y,
         function(z) {
           ## TO DO: this would get simpler if I just embraced NA for impossible
           ## stuff everywhere and propagated in all methods
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
#' @export
RC_to_A1 <- function(x) {
  stopifnot(is.character(x), all(grepl(.cr$is_R1C1_rx, x)))
  y <- R1C1_to_ra_ref(x)
  abs <- vapply(y, is_abs_ref, logical(1))
  if (any(!abs)) {
    warning("Ambiguous cell references ... NAs generated", call. = FALSE)
    ## TO DO: maybe I need to make a ra_ref with NAs everywhere instead?
    y[!abs] <- NA
  }
  vapply(y, to_string, character(1), fo = "A1")
}
