<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/jennybc/cellranger.svg?branch=master)](https://travis-ci.org/jennybc/cellranger) [![Coverage Status](https://coveralls.io/repos/jennybc/cellranger/badge.svg?branch=master)](https://coveralls.io/r/jennybc/cellranger?branch=master)

<img src="http://i.imgur.com/RJJy15I.jpg" width="270" align="right" />

Helper package to support R scripts or packages that interact with spreadsheets. Original development was motivated by [the wish to have a common interface](https://github.com/hadley/readxl/issues/8) for specifying cell ranges in [readxl](https://github.com/hadley/readxl) and [googlesheets](https://github.com/jennybc/googlesheets).

Anticipated [readxl](https://github.com/hadley/readxl) usage:

``` r
read_excel(..., range = "D12:F15")
read_excel(..., range = "R1C12:R6C15")
read_excel(..., range = cell_limits(c(1, 6), c(1, 15))
read_excel(..., range = cell_limits(c(2, NA), c(1, NA))
read_excel(..., range = cell_rows(1:100))
read_excel(..., range = cell_cols(3:8))
read_excel(..., range = cell_cols("B:MZ"))
read_excel(..., range = anchored("B4", dim = c(2, 10)))
```

### Range specification

The main goal is to translate Excel-like ranges, such as `A3:D7` or `R3C1:R7C4`, into something more programmatically useful. `cellranger` provides an S3 class, `cell_limits`, as the standard way to store a cell range. Construct `cell_limits` like so: `cell_limits(rows = c(ROW_MIN, ROW_MAX), cols = c(COL_MIN, COL_MAX))`.

``` r
library("cellranger")

(foo <- cell_limits(c(1, 3), c(1, 5)))
#> <cell_limits (1, 1) x (3, 5)>
```

The `print` method reports the cell rectangle as `(UPPER LEFT CELL) x (LOWER RIGHT CELL)` where cell locations are specifed as `(ROW, COL)`.

The `dim` method reports dimensions of the targetted cell rectangle. `as.range()` converts a `cell_limits` object back into an Excel range.

``` r
dim(foo)
#> rows cols 
#>    3    5

as.range(foo)
#> [1] "A1:E3"

as.range(foo, RC =TRUE)
#> [1] "R1C1:R3C5"
```

Use `NA` to leave a limit unspecified

``` r
cell_limits(c(NA, 7), c(3, NA))
#> <cell_limits (-, 3) x (7, -)>
```

#### Get a `cell_limits` object from an Excel range

``` r
as.cell_limits("C7")
#> <cell_limits (7, 3) x (7, 3)>

as.cell_limits("A1:D8")
#> <cell_limits (1, 1) x (8, 4)>

as.cell_limits("R2C3:R6C9")
#> <cell_limits (2, 3) x (6, 9)>
```

Recall the anticipated usage: `read_excel(..., range = "D12:F15")`. The intent is that `as.cell_limits()` will be called on user input for the `range =` argument.

#### Helpers for row- or column-only specification

``` r
cell_rows(1:100)
#> <cell_limits (1, -) x (100, -)>

cell_cols(3:8)
#> <cell_limits (-, 3) x (-, 8)>

cell_cols("B:MZ")
#> <cell_limits (-, 2) x (-, 364)>

cell_cols(c(NA, "AR"))
#> <cell_limits (-, -) x (-, 44)>
```

#### Specify the rectangle via an anchor cell

The rectangle can be described in terms of the upper left *anchor* cell and the dimensions of the rectangle, either directly or via an object (anticipates writing that object into the spreadsheet).

``` r
## direct specification of dimensions
anchored(anchor = "R4C2", dim = c(8, 2))
#> <cell_limits (4, 2) x (11, 3)>

as.range(anchored(anchor = "R4C2", dim = c(8, 2)), RC = TRUE)
#> [1] "R4C2:R11C3"

dim(anchored(anchor = "R4C2", dim = c(8, 2)))
#> rows cols 
#>    8    2

## indirect specification of dimensions, via the dimensions of an object
input <- head(iris)
anchored(input = input)
#> <cell_limits (1, 1) x (7, 5)>

as.range(anchored(input = input))
#> [1] "A1:E7"

dim(anchored(input = input))
#> rows cols 
#>    7    5
```

The `anchored()` function has additional arguments `header =` and `byrow =` for more control with 2-dimensional and 1-dimensional objects, respectively.

### Other helpers

We've exposed utility functions which could be useful to anyone manipulating Excel-like references.

``` r
## convert character column IDs to numbers ... and vice versa
letter_to_num(c('AA', 'ZZ', 'ABD', 'ZZZ', ''))
#> [1]    27   702   732 18278    NA

num_to_letter(c(27, 702, 732, 18278, 0, -5))
#> [1] "AA"  "ZZ"  "ABD" "ZZZ" NA    NA

## convert between A1 and R1C1 cell references
A1_to_RC(c("A1", "AZ10"))
#> [1] "R1C1"   "R10C52"

RC_to_A1(c("R1C1", "R10C52"))
#> [1] "A1"   "AZ10"
```
