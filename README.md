<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/jennybc/cellranger.svg?branch=master)](https://travis-ci.org/jennybc/cellranger) [![Coverage Status](https://coveralls.io/repos/jennybc/cellranger/badge.svg)](https://coveralls.io/r/jennybc/cellranger)

Helper package to support R scripts or packages that interact with spreadsheets. At this point, development is motivated by [the wish to have a common interface](https://github.com/hadley/readxl/issues/8) for specifying cell ranges in [readxl](https://github.com/hadley/readxl) and [googlesheets](https://github.com/jennybc/googlesheets).

Anticipated [readxl](https://github.com/hadley/readxl) usage:

``` r
read_excel(..., range = "D12:F15")
read_excel(..., range = "R1C12:R6C15")
read_excel(..., range = cell_limits(c(1, 6), c(1, 15))
read_excel(..., range = cell_limits(c(2, NA), c(1, NA))
```

### Range specification

The main goal is to translate Excel-like ranges, such as `A3:D7` or `R3C1:R7C4`, into something more programmatically useful. `cellranger` provides an S3 class, `cell_limits`, as the standard way to store a cell range. Functions are provided to convert various input formats into `cell_limits` objects.

``` r
library("cellranger")
cell_limits(c(1, 3), c(1, 5))
#> $rows
#> [1] 1 3
#> 
#> $cols
#> [1] 1 5
#> 
#> attr(,"class")
#> [1] "cell_limits"
cell_limits(c(NA, 7), c(3, NA))
#> $rows
#> [1] NA  7
#> 
#> $cols
#> [1]  3 NA
#> 
#> attr(,"class")
#> [1] "cell_limits"
as.cell_limits("C7")
#> $rows
#> [1] 7 7
#> 
#> $cols
#> [1] 3 3
#> 
#> attr(,"class")
#> [1] "cell_limits"
as.cell_limits("A1:D8")
#> $rows
#> [1] 1 8
#> 
#> $cols
#> [1] 1 4
#> 
#> attr(,"class")
#> [1] "cell_limits"
as.cell_limits("R2C3:R6C9")
#> $rows
#> [1] 2 6
#> 
#> $cols
#> [1] 3 9
#> 
#> attr(,"class")
#> [1] "cell_limits"
```

You can also convert a `cell_limits` object back into an Excel range.

``` r
rgCL <- cell_limits(rows = c(1, 4), cols = c(1, 3))
as.range(rgCL)
#> [1] "A1:C4"
as.range(rgCL, RC = TRUE)
#> [1] "R1C1:R4C3"
```

### Other helpers

We've exposed other helper functions which could be useful in other contexts, i.e. outside the targetted packages.

``` r
## convert character column IDs to numbers ... and vice versa
letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ'))
#> [1]    27   702   732 18278
num_to_letter(c(27, 702, 732, 18278))
#> [1] "AA"  "ZZ"  "ABD" "ZZZ"
## convert between A1 and R1C1 cell references
A1_to_RC(c("A1", "AZ10"))
#> [1] "R1C1"   "R10C52"
RC_to_A1(c("R1C1", "R10C52"))
#> [1] "A1"   "AZ10"
```
