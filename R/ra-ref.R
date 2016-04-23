#' ra_ref class
#'
#' The \code{ra_ref} class is used to represent a single relative, absolute, or
#' mixed cell reference, presumably found in a formula. When \code{row_abs} is
#' \code{TRUE}, it means that \code{row_ref} identifies a specific row in an
#' absolute sense. When \code{row_abs} is \code{FALSE}, it means that
#' \code{row_ref} holds a positive, zero, or negative offset relative to the
#' address of the cell containing the formula that contains the associated cell
#' reference. Ditto for \code{col_abs} and \code{col_ref}.
#'
#' A \code{ra_ref} object can also store the name of a sheet and a file, though
#' these will often be \code{NA}. A cell reference in a formula can potentially
#' be qualified like this: \code{[my_workbook.xlxs]Sheet1!R2C3}. In Testoft
#' (2014), he creates an entirely separate class for this, a \code{cell_ref},
#' which consists of a sheet- and file-ignorant \code{ra_ref} object and a sheet
#' reference (he doesn't allow formulas to refer to other files). I hope I
#' don't regret choosing a different path.
#'
#' @param row_ref integer, row or row offset
#' @param row_abs logical indicating whether \code{row_ref} is absolute or
#'   relative
#' @param col_ref integer, column or column offset
#' @param col_abs logical indicating whether \code{col_ref} is absolute or
#'   relative
#' @param sheet the name of a sheet (a.k.a. worksheet or tab)
#' @param file the name of a file (a.k.a. workbook)
#'
#' @return a \code{ra_ref} object
#' @export
#'
#' @template reference-sestoft
#'
#' @examples
#' ra_ref()
#' ra_ref(row_ref = 3, col_ref = 2)
#' ra_ref(row_ref = 10, row_abs = FALSE, col_ref = 3, col_abs = TRUE)
#' ra_ref(sheet = "a sheet")
ra_ref <- function(row_ref = 1L,
                   row_abs = TRUE,
                   col_ref = 1L,
                   col_abs = TRUE,
                   sheet = NA_character_,
                   file = NA_character_) {
  row_ref <- as.integer(row_ref)
  col_ref <- as.integer(col_ref)
  stopifnot(length(row_ref) == 1L, length(row_abs) == 1L,
            length(col_ref) == 1L, length(col_abs) == 1L,
            is.logical(row_abs), is.logical(col_abs),
            is.character(sheet), is.character(file),
            length(sheet) == 1, length(file) == 1)
  if ( (isTRUE(row_abs) && isTRUE(row_ref < 1)) ||
       (isTRUE(col_abs) && isTRUE(col_ref < 1)) ) {
    stop("Absolute row or column references must be >= 1:\n",
         " row_abs = ", row_abs, ", row_ref = ", row_ref, "\n",
         " col_abs = ", col_abs, ", col_ref = ", col_ref, "\n",
         call. = FALSE)
  }
  structure(list(row_ref = row_ref, row_abs = row_abs,
                 col_ref = col_ref, col_abs = col_abs,
                 sheet = sheet, file = file),
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
  ra_part <- c(`TRUE` = "abs", `FALSE` = "rel", `NA` = "NA")
  row_ra <- ra_part[as.character(x$row_abs)]
  col_ra <- ra_part[as.character(x$col_abs)]
  sheet_part <- paste0(" sheet: ", add_single_quotes(x$sheet), "\n")
  sheet_part <- if (is.na(x$sheet)) "" else sheet_part

  cat("<ra_ref>\n")
  cat("   row: ", x$row_ref, " (", row_ra, ")\n",
      "   col: ", x$col_ref, " (", col_ra, ")\n",
      sheet_part, sep = "")
  ## no printing of file name ... wait til I see it needed IRL
  cat(" ", to_string(x, fo = fo), "\n", sep = "")
}

#' Convert to a ra_ref object
#'
#' Convert various representations of a cell reference into an object of class
#' \code{\link{ra_ref}}.
#' \itemize{
#' \item \code{as.ra_ref} is NOT vectorized and therefore requires the input to
#' represent exactly one cell, i.e. be of length 1.
#' \item \code{as.ra_ref_v} accepts input of length >= 1 and returns a list of
#' \code{\link{ra_ref}} objects.
#' }
#'
#' @param x one or more cell references, as a character vector or
#' \code{\link{cell_addr}} object
#' @template param-ddd
#'
#' @return a \code{\link{ra_ref}} object, in the case of \code{as.ra_ref}, or a
#'   list of them, in the case of \code{as.ra_ref_v}
#' @name as.ra_ref
NULL

#' @rdname as.ra_ref
#' @export
as.ra_ref <- function(x, ...) UseMethod("as.ra_ref")

#' @rdname as.ra_ref
#' @export
as.ra_ref_v <- function(x, ...) UseMethod("as.ra_ref_v")

#' @rdname as.ra_ref
#' @template param-fo
#' @param warn logical, \code{TRUE} (default) requests a warning if a file or
#'   worksheet name is found in the string, since this cannot be represented in
#'   a \code{\link{ra_ref}} object and will be dropped.
#' @param strict logical, \code{TRUE} (default) indicates that, for references
#'   in "A1" format, only absolute references should be converted. If
#'   \code{FALSE} a purely relative "A1" reference, like B4, will be treated as
#'   purely absolute, i.e. like $B$4. Regardless of \code{strict}, a mixed "A1"
#'   reference will lead to \code{NA}(s) in the affected position(s).
#'
#' @examples
#' ## as.ra_ref.character()
#' as.ra_ref("$F$2")
#' as.ra_ref("R[-4]C3")
#' as.ra_ref("B4")
#' as.ra_ref("B4", strict = FALSE)
#' as.ra_ref("B$4")
#'
#' ## this is actually ambiguous! is format A1 or R1C1 format?
#' as.ra_ref("RC2")
#' ## format could be specified in this case
#' as.ra_ref("RC2", fo = "R1C1")
#' as.ra_ref("RC2", fo = "A1", strict = FALSE)
#'
#' @export
as.ra_ref.character <- function(x, fo = NULL, warn = TRUE, strict = TRUE, ...) {
  stopifnot(length(x) == 1L)
  parsed <- as.list(parse_as_ref_string(x)[1, , drop = TRUE])
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
    rar <- A1_to_ra_ref(ref, warn = warn, strict = strict)[[1]]
  } else {
    rar <- R1C1_to_ra_ref(ref)[[1]]
  }
  rar$sheet <- if (identical(parsed$sheet, "")) rar$sheet else parsed$sheet
  rar$file <- if (identical(parsed$file, "")) rar$file else parsed$file
  rar
}

#' @rdname as.ra_ref
#' @export
#' @examples
#' ## as.ra_ref_v.character()
#' cs <- c("$A$1", "Sheet1!$F$14", "Sheet2!B$4", "D9")
#' \dontrun{
#' ## won't work because as.ra_ref methods not natively vectorized
#' as.ra_ref(cs)
#' }
#' ## use as.ra_ref_v instead
#' as.ra_ref_v(cs, strict = FALSE, warn = FALSE)
as.ra_ref_v.character <- function(x, fo = NULL, warn = TRUE,
                                  strict = TRUE, ...) {
  lapply(x, as.ra_ref, fo = fo, warn = warn, strict = strict)
}

#' @rdname as.ra_ref
#' @export
#' @examples
#' ## as.ra_ref.cell_addr
#' ca <- cell_addr(2, 5)
#' as.ra_ref(ca)
as.ra_ref.cell_addr <- function(x, ...) {
  stopifnot(length(x) == 1L)
  ra_ref(row_ref = cell_row(x), row_abs = if (is.na(cell_row(x))) NA else TRUE,
         col_ref = cell_col(x), col_abs = if (is.na(cell_row(x))) NA else TRUE)
}

#' @rdname as.ra_ref
#' @export
#' @examples
#' ## as.ra_ref_v.cell_addr()
#'
#' ca <- cell_addr(1:3, 1)
#' \dontrun{
#' ## won't work because as.ra_ref methods not natively vectorized
#' as.ra_ref(ca)
#' }
#' ## use as.ra_ref_v instead
#' as.ra_ref_v(ca)
as.ra_ref_v.cell_addr <- function(x, ...) {
  mapply(ra_ref, row_ref = cell_row(x), col_ref = cell_col(x), SIMPLIFY = FALSE)
}
