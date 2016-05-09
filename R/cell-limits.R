#' Create a cell_limits object
#'
#' A \code{cell_limits} object is a list with three components:
#'
#' \itemize{
#'   \item \code{ul} vector specifying upper left cell of target rectangle, of
#'   the form \code{c(ROW_MIN, COL_MIN)}
#'   \item \code{lr} vector specifying lower right cell of target rectangle, of
#'   the form \code{c(ROW_MAX, COL_MAX)}
#'   \item \code{sheet} string specifying worksheet name, which may be
#'   \code{NA}, meaning it's unspecified
#' }
#'
#' A value of \code{NA} in \code{ul} or \code{lr} means the corresponding limit
#' is left unspecified. Therefore a verbose way to specify no limits at all
#' would be \code{cell_limits(c(NA, NA), c(NA, NA))}. If the maximum row or
#' column is specified but the associated minimum is not, then the minimum is
#' set to 1.
#'
#' When specified via character, cell references can be given in A1 or R1C1
#' notation and must be interpretable as absolute references. For A1, this means
#' either both row and column are annotated with a dollar sign \code{$} or
#' neither is. So, no mixed references, like \code{B$4}. For R1C1, this means no
#' square brackets, like \code{R[-3]C[3]}.
#'
#' @param ul vector identifying upper left cell of target rectangle
#' @param lr vector identifying lower right cell of target rectangle
#' @param sheet string containing worksheet name, optional
#' @param x input to convert into a \code{cell_limits} object
#'
#' @return a \code{cell_limits} object
#'
#' @examples
#' cell_limits(c(1, 3), c(1, 5))
#' cell_limits(c(NA, 7), c(3, NA))
#' cell_limits(c(NA, 7))
#' cell_limits(lr = c(3, 7))
#'
#' cell_limits(c(1, 3), c(1, 5), "Sheet1")
#' cell_limits(c(1, 3), c(1, 5), "Spaces are evil")
#'
#' dim(as.cell_limits("A1:F10"))
#'
#' @export
cell_limits <- function(ul = c(NA_integer_, NA_integer_),
                        lr = c(NA_integer_, NA_integer_),
                        sheet = NA_character_) {

  stopifnot(length(ul) == 2L, length(lr) == 2L,
            length(sheet) == 1L, is.character(sheet))

  ul <- as.integer(ul)
  lr <- as.integer(lr)

  NA_or_pos <- function(x) is.na(x) | x > 0
  stopifnot(all(NA_or_pos(ul)))
  stopifnot(all(NA_or_pos(lr)))

  if (is.na(ul[1]) && !is.na(lr[1])) ul[1] <- 1L
  if (is.na(ul[2]) && !is.na(lr[2])) ul[2] <- 1L

  rows <- c(ul[1], lr[1])
  cols <- c(ul[2], lr[2])

  if (!anyNA(rows)) stopifnot(rows[1] <= rows[2])
  if (!anyNA(cols)) stopifnot(cols[1] <= cols[2])

  structure(list(ul = ul, lr = lr, sheet = sheet),
            class = c("cell_limits", "list"))

}

#' @export
print.cell_limits <- function(x, ...) {
  ul <- ifelse(is.na(x$ul), "-", as.character(x$ul))
  lr <- ifelse(is.na(x$lr), "-", as.character(x$lr))
  sheet <- if (is.na(x$sheet)) "" else paste0(" in '", x$sheet, "'")

  cat("<cell_limits (", ul[1], ", ", ul[2], ") x (",
      lr[1], ", ", lr[2], ")", sheet, ">\n",
      sep = "")
}

#' @rdname cell_limits
#' @export
dim.cell_limits <- function(x) c(x$lr[1] - x$ul[1], x$lr[2] - x$ul[2]) + 1

#' @rdname cell_limits
#' @template param-ddd
#' @export
as.cell_limits <- function(x, ...) UseMethod("as.cell_limits")

#' @rdname cell_limits
#' @export
as.cell_limits.cell_limits <- function(x, ...) x

#' @rdname cell_limits
#' @export
as.cell_limits.NULL <- function(x, ...) cell_limits()

#' @rdname cell_limits
#' @template param-fo
#' @examples
#' as.cell_limits("A1")
#' as.cell_limits("$Q$24")
#' as.cell_limits("A1:D8")
#' as.cell_limits("R5C11")
#' as.cell_limits("R2C3:R6C9")
#' as.cell_limits("Sheet1!R2C3:R6C9")
#' as.cell_limits("'Spaces are evil'!R2C3:R6C9")
#'
#' \dontrun{
#' ## explicitly mixed A1 references won't work
#' as.cell_limits("A$2")
#' ## mixed or relative R1C1 references won't work
#' as.cell_limits("RC[4]")
#' }
#' @export
as.cell_limits.character <- function(x, fo = NULL, ...) {
  stopifnot(length(x) == 1L)
  parsed <- parse_ref_string(x, fo = fo)
  if (is.na(parsed$fo)) {
    stop("Can't guess format of this cell reference:\n", parsed$ref,
         call. = FALSE)
  }
  ## parsed$ref_v has length 1 or 2, depending on whether input was a range
  if (parsed$fo == "A1") {
    rar_list <- A1_to_ra_ref(parsed$ref_v, strict = FALSE)
  } else {
    rar_list <- R1C1_to_ra_ref(parsed$ref_v)
  }
  not_abs <- vapply(rar_list, is_not_abs_ref, logical(1))
  if (any(not_abs)) {
    stop("Mixed or relative cell references aren't allowed:\n",
         parsed$ref, call. = FALSE)
  }

  ## if single cell input --> duplicate that thing!
  rar_list <- rep_len(rar_list, 2)

  cell_limits(
    ul = rar_list[[1]][c("row_ref", "col_ref")],
    lr = rar_list[[2]][c("row_ref", "col_ref")],
    sheet = if (parsed$sheet == '') NA_character_ else parsed$sheet
  )
}

#' Convert a cell_limits object to a cell range
#'
#' @param x a cell_limits object
#' @template param-fo
#' @template param-strict
#' @template param-sheet
#'
#' @return length one character vector holding a cell range
#'
#' @examples
#' rgCL <- cell_limits(ul = c(1, 2), lr = c(7, 6))
#' as.range(rgCL)
#' as.range(rgCL, fo = "A1")
#'
#' rgCL_ws <- cell_limits(ul = c(1, 2), lr = c(7, 6), sheet = "A Sheet")
#' as.range(rgCL_ws)
#' as.range(rgCL_ws, fo = "A1")
#' @export
as.range <- function(x, fo = c("R1C1", "A1"), strict = FALSE, sheet = NULL) {
  stopifnot(inherits(x, "cell_limits"), isTOGGLE(strict), isTOGGLE(sheet))
  fo <- match.arg(fo)
  if (anyNA(unlist(x[c("ul", "lr")]))) return(NA_character_)
  ca <- cell_addr(c(x$ul[1], x$lr[1]), c(x$ul[2], x$lr[2]))
  range <- paste(to_string(ca, fo = fo, strict = strict), collapse = ":")
  sheet <- sheet %||% !is.na(x$sheet)
  if (sheet) {
    range <- paste(add_single_quotes(x$sheet), range, sep = "!")
  }
  range
}
