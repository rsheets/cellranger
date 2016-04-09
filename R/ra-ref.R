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
  structure(list(rowRef = rowRef, rowAbs = rowAbs,
                 colRef = colRef, colAbs = colAbs),
            class = c("ra_ref", "list"))
}

#' @export
print.ra_ref <- function(x, ...) {
  cat("<ra_ref>\n", sep = "")
  cat(paste0(" row: ", x$rowRef, " (", if (x$rowAbs) "abs" else "rel", ")\n",
             " col: ", x$colRef, " (", if (x$colAbs) "abs" else "rel", ")\n"))
  cat(" ", to_string(x), "\n", sep = "")
  cat(" ", to_string(x, fo = "R1C1"), "\n", sep = "")
}


#' Get string representation of cell and cell area references
#'
#' @param x an object of class \code{\link{ra_ref}} or blah blah
#' @param fo either \code{"A1"} (the default) or \code{"R1C1"} specifying the
#'   cell reference format
#'
#' @return a character vector of length one
#' @export
#'
#' @examples
#' to_string(ra_ref())
#' to_string(ra_ref(), fo = "R1C1")
#' to_string(ra_ref(rowRef = 3, colRef = 2))
#' to_string(ra_ref(rowRef = 10, rowAbs = FALSE, colRef = 3, colAbs = TRUE),
#'           fo = "R1C1")
to_string <- function(x, fo = c("A1", "R1C1")) UseMethod("to_string")

#' @export
to_string.ra_ref <- function(x, fo = c("A1", "R1C1")) {
  fo <- match.arg(fo)
  if (fo == "A1") {
    return(paste0(rel_abs_format(x$colAbs), num_to_letter(x$colRef),
                  rel_abs_format(x$rowAbs), x$rowRef))
  }
  paste0("R", rel_abs_format(x$rowAbs, x$rowRef, "R1C1"),
         "C", rel_abs_format(x$colAbs, x$colRef, "R1C1"))
}

#' Convert to a ra_ref object
#'
#' Convert something, usually a string, into an object of class
#' \code{\link{ra_ref}}.
#'
#' @param x something
#' @param ... potentially other stuff
#'
#' @return a \code{\link{ra_ref}} object
#'
#' @export
#'
#' @examples
#' as.ra_ref("D$4")
#' as.ra_ref("R[-4]C3")
as.ra_ref <- function(x, ...) UseMethod("as.ra_ref")

#' @export
as.ra_ref.character <- function(x, warn = TRUE, ...) {
  parsed <- parse_as_ref_string(x)
  if (!is.null(parsed$invalid)) {
    stop("Invalid string for a cell reference:\n", parsed$invalid,
         call. = FALSE)
  }
  if (warn && !all(is.null(unlist(parsed[c("fn", "wsn")])))) {
    warning("Can't store file and/or worksheet name in a ra_ref object:\n",
            parsed$input, call. = FALSE)
  }
  ref <- unlist(strsplit(parsed$cell_ref, ":"))
  if (length(ref) != 1L) {
    stop("Can't make a ra_ref object from a cell area reference:\n",
         parsed$cell_ref, call. = FALSE)
  }
  m <- c(A1 = grep(.cr$is_A1_rx, ref), R1C1 = grep(.cr$is_R1C1_rx, ref))
  if (length(m) < 1) {
    stop("Cell reference follows neither the A1 nor R1C1 format:\n",
         ref, call. = FALSE)
  }
  if (length(m) > 1) {
    ## this seems impossible to me but who knows???
    stop("Not clear if cell reference is in A1 or R1C1 format:\n",
         ref, call. = FALSE)
  }
  fo <- names(m)
  y <- extract_named_captures(
    ref,
    pattern = if (fo == "A1") .cr$A1_ncg_rx else .cr$R1C1_ncg_rx
  )
  y$rowAbs <- nzchar(y$rowAbs)
  y$colAbs <- nzchar(y$colAbs)
  if (fo == "R1C1") {
    y$rowAbs <- !y$rowAbs
    y$colAbs <- !y$colAbs
  }
  ra_ref(rowRef = as.integer(y$rowRef),
         rowAbs = y$rowAbs,
         colRef = if (fo == "A1") letter_to_num(y$colRef) else as.integer(y$colRef),
         colAbs = y$colAbs)
}
