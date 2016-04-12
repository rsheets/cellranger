cell_addr <- function(row, col) {
  stopifnot(length(row) == 1L, length(col) == 1L,
            is.numeric(row), is.numeric(col))
  row <- as.integer(row)
  col <- as.integer(col)
  if (row < 1 || col < 1) {
    stop("cell_addr objects give absolute row and column, must be >= 1:\n",
         " row = ", row, ", col = ", col, "\n", call. = FALSE)
  }
  structure(c(row = row, col = col), class = "cell_addr")
}

print.cell_addr <- function(x, ...) {
  cat("<cel_addr>\n")
  print(unclass(x), ...)
}

as.ra_ref.cell_addr <- function(ca, ...) {
  ra_ref(rowRef = ca["row"], rowAbs = TRUE, colRef = ca["col"], colAbs = TRUE)
}

to_string.cell_addr <- function(x, fo = c("R1C1", "A1")) {
  fo <- match.arg(fo)
  to_string(as.ra_ref(x), fo = fo)
}

as.cell_addr <- function(x, ...) UseMethod("as.cell_addr")

as.cell_addr.logical <- function(x, ...) NA

as.cell_addr.ra_ref <- function(x, ...) {
  if (!x$rowAbs || !x$colAbs) {
    stop("cell_addr objects give absolute row and column, not relative:\n",
         " rowAbs = ", x$rowAbs, ", rowRef = ", x$rowRef, "\n",
         " colAbs = ", x$colAbs, ", colRef = ", x$colRef, "\n",
         call. = FALSE)
  }
  cell_addr(row = x$rowRef, col = x$colRef)
}

as.cell_addr.character <- function(x, fo = NULL, warn = TRUE, ...) {
  raref <- as.ra_ref(x, fo = fo, warn = warn)
  if (any(is.na(raref))) {
    stop("cell_addr objects give absolute row and column, not relative:\n",
         x, call. = FALSE)
  }
  as.cell_addr(raref)
}
