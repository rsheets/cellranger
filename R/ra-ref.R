#' ra_ref class
#'
#' The \code{ra_ref} class is used to represent a single relative, absolute, or
#' mixed cell reference, presumably found in a formula. When \code{rowAbs} is
#' \code{TRUE}, it means that \code{rowRef} identifies a specific row in an
#' absolute sense. When \code{rowAbs} is \code{FALSE}, it means that
#' \code{rowRef} holds a positive, zero, or negative offset relative to the
#' address of the cell containing the formula that contains the associated cell
#' reference. Ditto for \code{colAbs} and \code{colRef}.
#'
#' @param rowRef integer, row
#' @param rowAbs logical indicating whether \code{rowRef} is absolute or
#'   relative
#' @param colRef integer, column
#' @param colAbs logical indicating whether \code{colRef} is absolute or
#'   relative
#'
#' @return a \code{ra_ref} object
#' @export
#'
#' @template reference-sestoft
#'
#' @examples
#' ra_ref()
#' ra_ref(rowRef = 3, colRef = 2)
#' ra_ref(rowRef = 10, rowAbs = FALSE, colRef = 3, colAbs = TRUE)
ra_ref <- function(rowRef = 1L,
                   rowAbs = TRUE,
                   colRef = 1L,
                   colAbs = TRUE) {
  rowRef <- as.integer(rowRef)
  colRef <- as.integer(colRef)
  stopifnot(length(rowRef) == 1L, length(rowAbs) == 1L,
            length(colRef) == 1L, length(colAbs) == 1L,
            is.logical(rowAbs), is.logical(colAbs))
  if ( (isTRUE(rowAbs) && isTRUE(rowRef < 1)) ||
       (isTRUE(colAbs) && isTRUE(colRef < 1)) ) {
    stop("Absolute row or column references must be >= 1:\n",
         " rowAbs = ", rowAbs, ", rowRef = ", rowRef, "\n",
         " colAbs = ", colAbs, ", colRef = ", colRef, "\n",
         call. = FALSE)
  }
  structure(list(rowRef = rowRef, rowAbs = rowAbs,
                 colRef = colRef, colAbs = colAbs),
            class = c("ra_ref", "list"))
}

#' Print ra_ref object
#'
#' @param x an object of class \code{\link{ra_ref}}
#'
#' @template param-fo
#' @template param-ddd
#'
#' @examples
#' (rar <- ra_ref(3, TRUE, 1, TRUE))
#' print(ra_ref(), fo = "A1")
#'
#' @export
print.ra_ref <- function(x, fo = c("R1C1", "A1"), ...) {
  fo <- match.arg(fo)
  row_ra <-
    switch(as.character(x$rowAbs), `TRUE` = "abs", `FALSE` = "rel", `NA` = "NA")
  col_ra <-
    switch(as.character(x$colAbs), `TRUE` = "abs", `FALSE` = "rel", `NA` = "NA")
  cat("<ra_ref>\n")
  cat(paste0(" row: ", x$rowRef, " (", row_ra, ")\n",
             " col: ", x$colRef, " (", col_ra, ")\n"))
  cat(" ", to_string(x, fo = fo), "\n")
}

#' Convert to a ra_ref object
#'
#' Convert various representations of a cell reference into an object of class
#' \code{\link{ra_ref}}. Note this function is NOT vectorized, but see the
#' examples.
#'
#' @param x a cell reference
#' @template param-ddd
#'
#' @return a \code{\link{ra_ref}} object
#'
#' @export
as.ra_ref <- function(x, ...) UseMethod("as.ra_ref")

#' @describeIn as.ra_ref Convert a string representation of a cell reference
#'   into an object of class \code{\link{ra_ref}}
#'
#' @template param-fo
#' @param warn logical, \code{TRUE} (default) requests a warning if a file or
#'   worksheet name is found in the string, since this cannot be represented in
#'   a \code{\link{ra_ref}} object and will be dropped.
#' @param strict logical, \code{TRUE} (default) indicates that, for references
#'   in "A1" format, only absolute references should be converted. If
#'   \code{FALSE} a purely relative "A1" reference, like B4, will be treated as
#'   purely absolute, i.e. like $B$4. Regardless of \code{strict}, a mixed
#'   "A1" reference will lead to \code{NA}(s) in the affected position(s).
#'
#' @examples
#' as.ra_ref("$F$2")
#' as.ra_ref("R[-4]C3")
#' as.ra_ref("B4")
#' as.ra_ref("B4", strict = FALSE)
#' as.ra_ref("B$4")
#'
#' \dontrun{
#' ## this is actually ambiguous! is format A1 or R1C1 format?
#' as.ra_ref("RC2")
#' ## format must be specified in this case
#' as.ra_ref("RC2", fo = "R1C1")
#' as.ra_ref("RC2", fo = "A1", strict = FALSE)
#' }
#'
#' cs <- c("$A$1", "$F$14")
#' \dontrun{
#' ## won't work because as.ra_ref methods not natively vectorized
#' as.ra_ref(cs)
#' }
#' ## but it's easy enough to do with Vectorize
#' f <- Vectorize(as.ra_ref, USE.NAMES = FALSE, SIMPLIFY = FALSE)
#' f(cs)
#' @export
as.ra_ref.character <- function(x, fo = NULL, warn = TRUE, strict = TRUE, ...) {
  stopifnot(length(x) == 1L)
  parsed <- parse_as_ref_string(x)
  if (warn && !all(is.null(unlist(parsed[c("fn", "wsn")])))) {
    warning("Can't store file and/or worksheet name in a ra_ref object:\n",
            parsed$input, call. = FALSE)
  }
  ref <- unlist(strsplit(parsed$ref, ":"))
  if (length(ref) != 1L) {
    stop("Can't make a ra_ref object from a cell area reference:\n",
         parsed$cell_ref, call. = FALSE)
  }

  if (is.null(fo)) {
    ## guess_fo will warn when it returns c("R1C1", "A1")
    ## so let's just honor the usual R1C1 default and get on with things
    fo <- guess_fo(ref)[1]
  } else {
    fo <- match.arg(fo, c("R1C1", "A1"))
  }

  if (fo == "A1") {
    A1_to_ra_ref(ref, strict = strict)[[1]]
  } else {
    R1C1_to_ra_ref(ref)[[1]]
  }
}
