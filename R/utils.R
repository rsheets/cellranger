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
