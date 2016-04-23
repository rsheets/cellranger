rm_dollar_signs <- function(x) gsub('$', '', x, fixed = TRUE)

char0_to_NA <- function(x) if (length(x) < 1) NA_character_ else x

isFALSE <- function(x) identical(x, FALSE)

isTOGGLE <- function(x) is.null(x) || isTRUE(x) || isFALSE(x)

isTRUE_v <- function(x) !is.na(x) & x

## https://twitter.com/kevin_ushey/status/710223546929119232
transpose <- function(list) do.call(Map, c(c, list))

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
  ra_ref(row_ref = x$row_ref, row_abs = TRUE, col_ref = x$col_ref, col_abs = TRUE)
}

extract_named_captures <- function(string, pattern) {
  stopifnot(length(string) == 1L, length(pattern) == 1L)
  regexpr_output <- regexpr(pattern, string, perl = TRUE)
  if (regexpr_output == -1) return("")
  start <- attr(regexpr_output, "capture.start")[1, , drop = TRUE]
  cl <- attr(regexpr_output, "capture.length")[1, , drop = TRUE]
  stop <- start + cl - 1
  out <- as.list(substring(string, start, stop))
  setNames(out, attr(regexpr_output, "capture.names"))
}

guess_fo <- function(x) {
  m <- c(R1C1 = grep(.cr$is_R1C1_rx, x), A1 = grep(.cr$is_A1_rx, x))
  if (length(m) < 1) {
    warning("Cell reference follows neither the A1 nor R1C1 format:\n",
            x, "\nPutative format is NA.", call. = FALSE)
    return(NA_character_)
  }
  if (length(m) > 1) {
    ## OMFG this can actually happen. Example: RCx
    warning("Not clear if cell reference is in A1 or R1C1 format:\n",
            x, "\nSpecify format via `fo` argument.\n",
            "Putative format is c(\"R1C1\", \"A1\"), which is probably ",
            "not what you want.", call. = FALSE)
  }
  names(m)
}
