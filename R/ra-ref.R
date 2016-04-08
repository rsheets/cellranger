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

print.ra_ref <- function(x, ...) {
  cat("<ra_ref>\n", sep = "")
  cat(paste0(" ", if (x$rowAbs) "abs " else "rel ", "row: ", x$rowRef, "\n",
             " ", if (x$colAbs) "abs " else "rel ", "col: ", x$colRef, "\n"))
}

as.ra_ref <- function(x, ...) UseMethod("as.ra_ref")

as.ra_ref.character <- function(x, warn = TRUE, ...) {
  parsed <- parse_as_ref_string(x)
  if (!is.na(parsed["invalid"])) {
    stop(sprintf("Invalid string for a cell reference: %s", parsed["invalid"]))
  }
  if (warn && !all(is.na(parsed[c("fn", "wsn")]))) {
    warning(paste("File and/or worksheet name dropped while converting this",
                  "cell reference:\n", parsed["input"]), call. = FALSE)
  }
  parsed
}

as_ref_string <- function(x, fo = c("A1", "R1C1")) UseMethod("as_ref_string")

as_ref_string.ra_ref <- function(x, fo = c("A1", "R1C1")) {

  fo <- match.arg(fo)
  if (fo == "A1") {
    return(paste0(rel_abs_format_A1(x$colAbs), num_to_letter(x$colRef),
                  rel_abs_format_A1(x$rowAbs), x$rowRef))
  }
  paste0("R", rel_abs_format_RC(x$rowAbs, x$rowRef),
         "C", rel_abs_format_RC(x$colAbs, x$colRef))
}
