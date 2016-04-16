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
  stopifnot(length(rowRef) == 1L, length(rowAbs) == 1L,
            length(colRef) == 1L, length(colAbs) == 1L,
            is.numeric(rowRef), is.numeric(colRef),
            is.logical(rowAbs), is.logical(colAbs))
  rowRef <- as.integer(rowRef)
  colRef <- as.integer(colRef)
  if ( (rowAbs && rowRef < 1) ||
       (colAbs && colRef < 1) ) {
    stop("Absolute row or column references must be >= 1:\n",
         " rowAbs = ", rowAbs, ", rowRef = ", rowRef, "\n",
         " colAbs = ", colAbs, ", colRef = ", colRef, "\n",
         call. = FALSE)
  }
  structure(list(rowRef = rowRef, rowAbs = rowAbs,
                 colRef = colRef, colAbs = colAbs),
            class = c("ra_ref", "list"))
}

#' @export
print.ra_ref <- function(x, ...) {
  cat("<ra_ref>\n")
  cat(paste0(" row: ", x$rowRef, " (", if (x$rowAbs) "abs" else "rel", ")\n",
             " col: ", x$colRef, " (", if (x$colAbs) "abs" else "rel", ")\n"))
  cat(" ", to_string(x), "\n")
}

#' Convert to a ra_ref object
#'
#' Convert various representations of a cell reference into an object of class
#' \code{\link{ra_ref}}.
#'
#' @param x a cell reference
#' @param ... other arguments passed along to methods
#'
#' @return a \code{\link{ra_ref}} object
#'
#' @export
as.ra_ref <- function(x, ...) UseMethod("as.ra_ref")

#' @describeIn as.ra_ref Convert a string representation of a cell reference
#'   into an object of class \code{\link{ra_ref}}
#'
#' @param fo Optional specification of the cell reference format of the string
#'   \code{x}. If given, it must be either "A1" or "R1C1"; it can usually be
#'   inferred.
#' @param warn Logical, requests a warning if a file or worksheet name is found
#'   in the string, since this cannot be represented in a \code{\link{ra_ref}}
#'   object and will be dropped.
#' @param strict logical, indicates that only absolute references should be
#'   converted; defaults to \code{TRUE}
#'
#' @examples
#' as.ra_ref("$F$2")
#' as.ra_ref("R[-4]C3")
#'
#' \dontrun{
#' as.ra_ref("D$4") ## won't work because column ref is relative
#' as.ra_ref("D$4", strict = FALSE) ## let's pretend it's absolute!
#'
#' ## this is actually ambiguous! is format A1 or R1C1 format?
#' as.ra_ref("RC2")
#' ## format must be specified in this case
#' as.ra_ref("RC2", fo = "R1C1")
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
    m <- c(A1 = grep(.cr$is_A1_rx, ref), R1C1 = grep(.cr$is_R1C1_rx, ref))
    if (length(m) < 1) {
      stop("Cell reference follows neither the A1 nor R1C1 format:\n",
           ref, call. = FALSE)
    }
    if (length(m) > 1) {
      ## example: RCx
      stop("Not clear if cell reference is in A1 or R1C1 format:\n",
           ref, "\nSpecify format via `fo` argument.\n", call. = FALSE)
    }
    fo <- names(m)
  } else {
    fo <- match.arg(fo, c("R1C1", "A1"))
  }

  if (fo == "A1") {
    A1_to_ra_ref(ref, strict = strict)[[1]]
  } else {
    R1C1_to_ra_ref(ref)[[1]]
  }
}
