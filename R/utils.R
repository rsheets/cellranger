rm_dollar_signs <- function(x) gsub('$', '', x, fixed = TRUE)

char0_to_NA <- function(x) if (length(x) < 1) NA_character_ else x

isTOGGLE <- function(x) is.null(x) || isTRUE(x) || identical(x, FALSE)

add_single_quotes <- function(x) {
  if (grepl("\\s+", x)) {
    x <- paste0("'", x, "'")
  }
  x
}

remove_single_quotes <- function(x) gsub("^'|'$", "", x)

rel_abs_format_RC <- function(indAbs, rcRef) {
  if (indAbs) rcRef else paste0("[", rcRef, "]")
}

rel_abs_format_A1 <- function(indAbs) {
  if (indAbs) "$" else ""
}

extract_matches <- function(input, regexpr_output) {
  stopifnot(length(input) == 1L, length(regexpr_output) == 1L)
  if (input == -1) return("")
  start <- attr(regexpr_output, "capture.start")[1, , drop = TRUE]
  cl <- attr(regexpr_output, "capture.length")[1, , drop = TRUE]
  stop <- start + cl - 1
  out <- as.list(substring(input, start, stop))
  setNames(out, attr(regexpr_output, "capture.names"))
}
