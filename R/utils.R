char0_to_NA <- function(x) if (length(x) < 1) NA_character_ else x

isFALSE <- function(x) identical(x, FALSE)

isTOGGLE <- function(x) is.null(x) || isTRUE(x) || isFALSE(x)

isTRUE_v <- function(x) !is.na(x) & x

## https://twitter.com/kevin_ushey/status/710223546929119232
transpose <- function(list) do.call(Map, c(c, list))

## from
## https://github.com/hadley/purrr/blob/9534c29411f4ec262995498b0c2a78d0a619eae4/R/utils.R#L149-L155
## among other places
`%||%` <- function(x, y) if (is.null(x)) y else x

add_single_quotes <- function(x) {
  if (grepl("\\s+", x)) {
    x <- paste0("'", x, "'")
  }
  x
}

remove_single_quotes <- function(x) gsub("^'|'$", "", x)

rel_abs_format <- function(is_abs, rc_ref, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  if (fo == "A1") return(if (isTRUE_v(is_abs)) "$" else "")
  ## R1C1 case:
  if (isTRUE_v(is_abs)) return(rc_ref)
  ## unfortunate convention where R and C are used instead of R[0] and C[0]
  if (!is.na(rc_ref) && rc_ref == 0) "" else paste0("[", rc_ref, "]")
}

is_abs_ref <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  isTRUE(x$row_abs) && isTRUE(x$col_abs)
}

is_rel_ref <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  isFALSE(x$row_abs) && isFALSE(x$col_abs)
}

is_not_abs_ref <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  !isTRUE(x$row_abs) || !isTRUE(x$col_abs)
}

absolutize <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  x$row_abs <- x$col_abs <- TRUE
  x
}

relativize <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  x$row_abs <- x$col_abs <- FALSE
  x
}
