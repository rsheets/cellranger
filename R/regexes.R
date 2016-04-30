.cr <- new.env(parent = emptyenv())

## for parsing cell (area) references that are possibly qualified by
## file and/or worksheet name
.cr$filename_rx = "(?:^\\[([^\\]]+)\\])?"
.cr$worksheetname_rx <- "(?:'?([^'!]+)'?!)?"
.cr$ref_rx <- "([a-zA-Z0-9:\\-$\\[\\]]+)"
.cr$string_rx <- sprintf("^(?:%s%s%s|(.*))$", .cr$filename_rx,
                         .cr$worksheetname_rx, .cr$ref_rx)

parse_ref_string <- function(x, fo = NULL) {
  parsed <- as.list(rematch::re_match(.cr$string_rx, x)[1, , drop = TRUE])
  names(parsed) <- c("input", "file", "sheet", "ref", "invalid")
  parsed$ref_v <- unlist(strsplit(parsed$ref, ":"))
  stopifnot(length(parsed$ref_v) %in% 1:2)
  if (is.null(fo)) {
    fo_v <- lapply(parsed$ref_v, guess_fo)
    ## guess_fo will warn when it returns c("R1C1", "A1")
    ## so let's just honor the usual R1C1 default and get on with things
    fo_v <- vapply(fo_v, `[[`, character(1), 1)
    parsed$fo <- unique(fo_v)
    if (length(parsed$fo) > 1) {
      stop("Can't tell if cell references are in A1 or R1C1 format:\n",
           parsed$ref, call. = FALSE)
    }
  } else {
    parsed$fo <- match.arg(fo, c("R1C1", "A1"))
  }
  parsed
}

## for validating single cell references
.cr$is_A1_rx <- "^\\$?[A-Z]{1,3}\\$?[0-9]+$"
.cr$is_R1C1_rx <- "^R\\[?[0-9\\-]*\\]?C\\[?[0-9\\-]*\\]?$"

## for parsing single cell references
.cr$A1_ncg_rx <-
  "(?P<col_abs>\\$?)(?P<col_ref>[A-Z]+)(?P<row_abs>\\$?)(?P<row_ref>[0-9]+)"
.cr$R1C1_ncg_rx <-
  paste0("^R(?P<row_abs>\\[?)(?P<row_ref>[0-9\\-]*)(?:\\]?)",
          "C(?P<col_abs>\\[?)(?P<col_ref>[0-9\\-]*)(?:\\]?)$")

