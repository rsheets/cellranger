.cr <- new.env(parent = emptyenv())

## for parsing cell (area) references that are possibly qualified by
## file and/or worksheet name
.cr$filename_rx = "(?:^\\[([^\\]]+)\\])?"
.cr$worksheetname_rx <- "(?:'?([^'!]+)'?!)?"
.cr$ref_rx <- "([a-zA-Z0-9:\\-$\\[\\]]+)"
.cr$string_rx <- sprintf("^(?:%s%s%s|(.*))$", .cr$filename_rx,
                         .cr$worksheetname_rx, .cr$ref_rx)

## returns a character matrix
## one row per element of x
## columns are as named below
parse_as_ref_string <- function(x) {
  params <- rematch::re_match(.cr$string_rx, x)
  colnames(params) <- c("input", "file", "sheet", "ref", "invalid")
  params
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

