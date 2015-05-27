# cellranger 0.2

  * Added a `NULL` method for `as.cell_limits` generic so that `as.cell_limits(NULL)` returns default, degenerate cell limits, i.e. the min and max for rows and columns are uniformly `NA`.

  * A `cell_limits` object now inherits from "list".

# cellranger 0.1.0

  * Initial CRAN release
