.cr <- new.env(parent = emptyenv())

## for parsing cell (area) references that are possibly qualified by filename
## and/or worksheet
.cr$filename_rx = "(?:^\\[([^\\]]+)\\])?"
.cr$worksheetname_rx <- "(?:'?([^'!]+)'?!)?"
.cr$ref_rx <- "([a-zA-Z0-9:\\-$\\[\\]]+)"
.cr$string_rx <- sprintf("^(?:%s%s%s|(.*))$", .cr$filename_rx,
                         .cr$worksheetname_rx, .cr$ref_rx)

## for validating single cell references
.cr$is_A1_rx <- "^\\$?[A-Z]{1,3}\\$?[0-9]+$"
.cr$is_R1C1_rx <- "^R\\[?[0-9\\-]+\\]?C\\[?[0-9\\-]+\\]?$"

## for parsing single cell references
.cr$A1_ncg_rx <-
  "(?P<colAbs>\\$?)(?P<colRef>[A-Z]+)(?P<rowAbs>\\$?)(?P<rowRef>[0-9]+)"
.cr$R1C1_ncg_rx <-
  paste0("^R(?P<rowAbs>\\[?)(?P<rowRef>[0-9\\-]+)(?:\\]?)",
          "C(?P<colAbs>\\[?)(?P<colRef>[0-9\\-]+)(?:\\]?)$")
