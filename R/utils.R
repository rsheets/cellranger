rm_dollar_signs <- function(x) gsub('$', '', x, fixed = TRUE)

char0_to_NA <- function(x) if (length(x) < 1) NA_character_ else x

isTOGGLE <- function(x) is.null(x) || isTRUE(x) || identical(x, FALSE)

## https://twitter.com/kevin_ushey/status/710223546929119232
transpose <- function(list) do.call(Map, c(c, list))

add_single_quotes <- function(x) {
  if (grepl("\\s+", x)) {
    x <- paste0("'", x, "'")
  }
  x
}

remove_single_quotes <- function(x) gsub("^'|'$", "", x)

rel_abs_format <- function(indAbs, rcRef, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  if (fo == "A1") return(if (indAbs) "$" else "")
  ## R1C1 case:
  if (indAbs) return(rcRef)
  ## unfortunate convention where R and C are used instead of R[0] and C[0]
  if (rcRef == 0) "" else paste0("[", rcRef, "]")
}

is_abs_ref <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  x$rowAbs && x$colAbs
}

is_rel_ref <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  !x$rowAbs && !x$colAbs
}

is_mixed_ref <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  xor(x$rowAbs, x$colAbs)
}

absolutize <- function(x) {
  stopifnot(inherits(x, "ra_ref"))
  ra_ref(rowRef = x$rowRef, rowAbs = TRUE, colRef = x$colRef, colAbs = TRUE)
}

stopifnot_abs <- function(x, ...) UseMethod("stopifnot_abs")

stopifnot_abs.ra_ref <- function(x, ...) {
  if (!x$rowAbs || !x$colAbs) {
    printed_x <- capture.output(print(x))
    stop("Operation doesn't make sense for a relative cell reference:\n",
         paste(printed_x, collapse = "\n"), call. = FALSE)
  }
  x
}

stopifnot_abs.character <- function(x, ...) {
  n_dollar_signs <- length(gregexpr("\\$", x)[[1]])
  if (n_dollar_signs < 2) {
    stop("Operation doesn't make sense for a relative cell reference:\n", x,
         call. = FALSE)
  }
  x
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
