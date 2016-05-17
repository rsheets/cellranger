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
#' @template param-strict
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
as.ra_ref.character <- function(x, fo = NULL, strict = TRUE, ...) {
  stopifnot(is.character(x))
  if (length(x) > 1) {
    stop("Input must have length 1. Maybe you want the vectorized as.ra_ref_v()?")
  }
  as.ra_ref_v(x, fo = fo, strict = strict)[[1]]
}

#' @rdname as.ra_ref
#' @export
#' @examples
#' ## as.ra_ref_v.character()
#' cs <- c("$A$1", "Sheet1!$F$14", "Sheet2!B$4", "D9")
#' \dontrun{
#' ## won't work because as.ra_ref requires length one input
#' as.ra_ref(cs)
#' }
#' ## use as.ra_ref_v instead
#' as.ra_ref_v(cs, strict = FALSE)
as.ra_ref_v.character <- function(x, fo = NULL, strict = TRUE, ...) {
  parsed <- rematch::re_match(.cr$string_rx, x)
  colnames(parsed) <- c("input", "file", "sheet", "ref", "invalid")
  is_range <- grepl(":", parsed[ , "ref"])
  if (any(is_range)) {
    stop("Cell ranges not allowed here.\n", call. = FALSE)
  }
  if (is.null(fo)) {
    fo <- unique(guess_fo(parsed[ , "ref"]))
    if ("R1C1" %in% fo && "A1" %in% fo) {
      ## TODO? be willing to handle a mix of A1 and R1C1 refs
      stop("Cell references aren't uniformly A1 or R1C1 format:\n",
           call. = FALSE)
    }
    if (anyNA(fo) && length(fo) > 1) {
      ## (A1, NA) --> A1, (R1C1, NA) --> R1C1, NA --> NA
      fo <- fo[!is.na(fo)]
    }
  }
  if (identical(fo, "A1")) {
    rar <- A1_to_ra_ref(parsed[ , "ref"], strict = strict)
    if (anyNA(vapply(rar, `[[`, integer(1), "row_ref")) ||
        anyNA(vapply(rar, `[[`, integer(1), "col_ref"))) {
      warning("Non-absolute A1-formatted reference ... NAs generated",
              call. = FALSE)
    }
  } else { ## catches fo = "R1C1" and fo = NA
    rar <- R1C1_to_ra_ref(parsed[ , "ref"])
  }
  has_sheet <- nzchar(parsed[ , "sheet"])
  rar[has_sheet] <- mapply(function(x, sheet) {x$sheet <- sheet; x},
                           rar[has_sheet], parsed[has_sheet, "sheet"],
                           SIMPLIFY = FALSE)
  has_file <- nzchar(parsed[ , "file"])
  rar[has_file] <- mapply(function(x, file) {x$file <- file; x},
                           rar[has_file], parsed[has_file, "file"],
                           SIMPLIFY = FALSE)
  rar
}

#' @rdname as.ra_ref
#' @export
#' @examples
#' ## as.ra_ref.cell_addr
#' ca <- cell_addr(2, 5)
#' as.ra_ref(ca)
as.ra_ref.cell_addr <- function(x, ...) {
  stopifnot(length(x) == 1L)
  ra_ref(row_ref = addr_row(x), row_abs = if (is.na(addr_row(x))) NA else TRUE,
         col_ref = addr_col(x), col_abs = if (is.na(addr_row(x))) NA else TRUE)
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
  mapply(ra_ref, row_ref = addr_row(x), col_ref = addr_col(x), SIMPLIFY = FALSE)
}
