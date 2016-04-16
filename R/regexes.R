.cr <- new.env(parent = emptyenv())

## for parsing cell (area) references that are possibly qualified by
## file and/or worksheet name
.cr$filename_rx = "(?:^\\[([^\\]]+)\\])?"
.cr$worksheetname_rx <- "(?:'?([^'!]+)'?!)?"
.cr$ref_rx <- "([a-zA-Z0-9:\\-$\\[\\]]+)"
.cr$string_rx <- sprintf("^(?:%s%s%s|(.*))$", .cr$filename_rx,
                         .cr$worksheetname_rx, .cr$ref_rx)

## for validating single cell references
.cr$is_A1_rx <- "^\\$?[A-Z]{1,3}\\$?[0-9]+$"
.cr$is_R1C1_rx <- "^R\\[?[0-9\\-]*\\]?C\\[?[0-9\\-]*\\]?$"

## for parsing single cell references
.cr$A1_ncg_rx <-
  "(?P<colAbs>\\$?)(?P<colRef>[A-Z]+)(?P<rowAbs>\\$?)(?P<rowRef>[0-9]+)"
.cr$R1C1_ncg_rx <-
  paste0("^R(?P<rowAbs>\\[?)(?P<rowRef>[0-9\\-]*)(?:\\]?)",
          "C(?P<colAbs>\\[?)(?P<colRef>[0-9\\-]*)(?:\\]?)$")

parse_as_ref_string <- function(x, must_work = TRUE) {
  param_names <- c("fn", "wsn", "ref", "invalid")
  replace <-
    stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <-
    lapply(replace, function(r) gsub(.cr$string_rx, r, x, perl = TRUE))
  if (must_work && nzchar(params$invalid)) {
    stop("Invalid string for a cell reference:\n", params$invalid,
         call. = FALSE)
  }
  params$input <- x
  params[nzchar(params, keepNA = TRUE)]
}
