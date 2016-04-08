.cr <- new.env(parent = emptyenv())
.cr$filename_rx = "(?:^\\[([^\\]]+)\\])?"
.cr$worksheetname_rx <- "(?:'?([^'!]+)'?!)?"
.cr$ref_rx <- "([a-zA-Z0-9:$\\[\\]]*)"
.cr$string_rx <- sprintf("^(?:%s%s%s|(.*))$", .cr$filename_rx,
                         .cr$worksheetname_rx, .cr$ref_rx)

parse_as_ref_string <- function(x) {
  param_names <- c("fn", "wsn", "cell_ref", "invalid")
  replace <-
    stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <-
    vapply(replace,
           function(r) gsub(.cr$string_rx, r, x, perl = TRUE), character(1))
  params["input"] <- x
  # if (params$invalid != "")
  #   stop(sprintf("Invalid string for the range: %s", x))
  params[nzchar(unlist(params), keepNA = TRUE)]
}
