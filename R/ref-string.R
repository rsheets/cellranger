parse_as_ref_string <- function(x) {
  param_names <- c("fn", "wsn", "cell_ref", "invalid")
  replace <-
    stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <-
    lapply(replace, function(r) gsub(.cr$string_rx, r, x, perl = TRUE))
  params$input <- x
  params[nzchar(params, keepNA = TRUE)]
}
