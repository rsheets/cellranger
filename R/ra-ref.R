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
  cat(paste0(" row: ", x$rowRef, " (", if (x$rowAbs) "abs" else "rel", ")\n",
             " col: ", x$colRef, " (", if (x$colAbs) "abs" else "rel", ")\n"))
  cat(" ", to_string(x), "\n", sep = "")
  cat(" ", to_string(x, fo = "R1C1"), "\n", sep = "")
}

to_string <- function(x, fo = c("A1", "R1C1")) UseMethod("to_string")

to_string.ra_ref <- function(x, fo = c("A1", "R1C1")) {
  fo <- match.arg(fo)
  if (fo == "A1") {
    return(paste0(rel_abs_format(x$colAbs), num_to_letter(x$colRef),
                  rel_abs_format(x$rowAbs), x$rowRef))
  }
  paste0("R", rel_abs_format(x$rowAbs, x$rowRef, "R1C1"),
         "C", rel_abs_format(x$colAbs, x$colRef, "R1C1"))
}

as.ra_ref <- function(x, ...) UseMethod("as.ra_ref")

as.ra_ref.character <- function(x, warn = TRUE, ...) {
  parsed <- parse_as_ref_string(x)
  if (!is.null(parsed$invalid)) {
    stop("Invalid string for a cell reference:\n", parsed$invalid,
         call. = FALSE)
  }
  if (warn && !all(is.null(parsed[c("fn", "wsn")]))) {
    warning("Can't store file and/or worksheet name in a ra_ref object:\n",
            parsed$input, call. = FALSE)
  }
  ref <- unlist(strsplit(parsed$cell_ref, ":"))
  if (length(ref) != 1L) {
    stop("Can't make a ra_ref object from a cell area reference:\n",
         parsed$cell_ref, call. = FALSE)
  }
  m <- c(A1 = grep(.cr$is_A1_rx, ref), R1C1 = grep(.cr$is_R1C1_rx, ref))
  if (length(m) < 1) {
    stop("Cell reference follows neither the A1 nor R1C1 format:\n",
         ref, call. = FALSE)
  }
  if (length(m) > 1) {
    ## this seems impossible to me but who knows???
    stop("Not clear if cell reference is in A1 or R1C1 format:\n",
         ref, call. = FALSE)
  }
  fo <- names(m)
  y <- extract_named_captures(
    ref,
    pattern = if (fo == "A1") .cr$A1_ncg_rx else .cr$R1C1_ncg_rx
  )
  y$rowAbs <- nzchar(y$rowAbs)
  y$colAbs <- nzchar(y$colAbs)
  if (fo == "R1C1") {
    y$rowAbs <- !y$rowAbs
    y$colAbs <- !y$colAbs
  }
  ra_ref(rowRef = as.integer(y$rowRef),
         rowAbs = as.logical(y$rowAbs),
         colRef = if (fo == "A1") letter_to_num(y$colRef) else y$colRef,
         colAbs = as.logical(y$colAbs))
}
