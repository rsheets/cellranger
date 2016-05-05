context("cell specification")

test_that("Cell range is converted to a cell_limit object and vice versa", {

  rgA1 <- "A1:C4"
  rgRC <- "R1C1:R4C3"
  rgCL <- cell_limits(ul = c(1, 1), lr = c(4, 3))
  expect_equal(as.cell_limits(rgA1), rgCL)
  expect_equal(as.cell_limits(rgRC), rgCL)
  expect_equal(as.range(rgCL), rgRC)
  expect_equal(as.range(rgCL, fo = "A1"), rgA1)

  rgA1sheet <- "sheet!A1:C4"
  rgRCsheet <- "sheet!R1C1:R4C3"
  rgCLwsn <- cell_limits(ul = c(1, 1), lr = c(4, 3), sheet = "sheet")
  expect_equal(as.cell_limits(rgA1sheet), rgCLwsn)
  expect_equal(as.cell_limits(rgRCsheet), rgCLwsn)
  expect_equal(as.range(rgCLwsn), rgRCsheet)
  expect_equal(as.range(rgCLwsn, sheet = FALSE), rgRC)
  expect_equal(as.range(rgCLwsn, fo = "A1"), rgA1sheet)

  rgA1 <- "E7"
  rgA1A1 <- "E7:E7"
  rgRC <- "R7C5"
  rgRCRC <- "R7C5:R7C5"
  rgCL <- cell_limits(ul = c(7, 5), lr = c(7, 5))
  expect_equal(as.cell_limits(rgA1), rgCL)
  expect_equal(as.cell_limits(rgRC), rgCL)
  expect_equal(as.cell_limits(rgA1A1), rgCL)
  expect_equal(as.cell_limits(rgRCRC), rgCL)
  expect_equal(as.range(rgCL), rgRCRC)
  expect_equal(as.range(rgCL, fo = "A1"), rgA1A1)

  rgA1sheet <- "sheet!E7"
  rgA1A1sheet <- "sheet!E7:E7"
  rgRCsheet <- "sheet!R7C5"
  rgRCRCsheet <- "sheet!R7C5:R7C5"
  rgCLsheet <- cell_limits(ul = c(7, 5), lr = c(7, 5), sheet = "sheet")
  expect_equal(as.cell_limits(rgA1sheet), rgCLsheet)
  expect_equal(as.cell_limits(rgRCsheet), rgCLsheet)
  expect_equal(as.cell_limits(rgA1A1sheet), rgCLsheet)
  expect_equal(as.cell_limits(rgRCRCsheet), rgCLsheet)
  expect_equal(as.range(rgCLsheet), rgRCRCsheet)
  expect_equal(as.range(rgCLsheet, fo = "A1"), rgA1A1sheet)

  rgCL <- cell_limits(ul = c(NA, 1), lr = c(4, NA))
  expect_true(is.na(as.range(rgCL)))

})

test_that("Whitespace-containing sheet names gain/lose single quotes", {
  x <- cell_limits(ul = c(1, 1), lr = c(4, 3), sheet = "aaa bbb")
  expect_identical(as.range(x), "'aaa bbb'!R1C1:R4C3")
  expect_identical(as.cell_limits("'aaa bbb'!R1C1:R4C3"), x)
})

test_that("Bad cell ranges throw errors", {

  expect_warning(expect_error(as.cell_limits("eggplant")))
  expect_warning(expect_error(as.cell_limits("A:B10")))
  expect_warning(expect_error(as.cell_limits(":B10")))
  expect_error(as.cell_limits("A1:R3C3"))
  expect_error(as.cell_limits("A1:B2:C3"))
  expect_warning(expect_error(as.cell_limits("14:17")))
  expect_error(as.cell_limits(14:17))
  expect_error(as.cell_limits(B2:D9))
  expect_error(cell_limits(ul = c(-1, 1), lr = c(3, 4)))
  expect_error(cell_limits(ul = c(0, 1), lr = c(3, 4)))
  expect_error(cell_limits(ul = c(1, 4), lr = c(3, 1)))

})

test_that("Degenerate, all-NA input is tolerated", {

  cl <- cell_limits()
  expect_is(cl, "cell_limits")
  expect_is(cl$ul, "integer")

  cl2 <- cell_limits(c(NA, NA))
  expect_identical(cl, cl2)

  cl3 <- cell_limits(lr = c(NA, NA))
  expect_identical(cl, cl3)

})

test_that("as.cell_limits can operate on NULL input", {

  expect_identical(as.cell_limits(NULL), cell_limits())

})

test_that("cell_limits objects inherit from list", {

  expect_is(cell_limits(), "list")

})

test_that("Row-only specifications work", {

  expect_identical(cell_rows(c(NA, NA)), cell_limits())
  expect_identical(cell_rows(c(NA, 3)), cell_limits(lr = c(3, NA)))
  expect_identical(cell_rows(c(7, NA)), cell_limits(c(7, NA)))
  expect_identical(cell_rows(c(3, NA, 10)), cell_limits(c(3, NA), c(10, NA)))
  expect_identical(cell_rows(c(10, NA, 3)), cell_limits(c(3, NA), c(10, NA)))
  expect_identical(cell_rows(4:16), cell_limits(c(4, NA), c(16, NA)))
  expect_error(cell_rows(c(7, 2)))

})

test_that("Column-only specifications work", {

  expect_identical(cell_cols(c(NA, NA)), cell_limits())
  expect_identical(cell_cols(c(NA, 3)), cell_limits(lr = c(NA, 3)))
  expect_identical(cell_cols(c(7, NA)), cell_limits(c(NA, 7)))
  expect_identical(cell_cols(c(3, NA, 10)), cell_limits(c(NA, 3), c(NA, 10)))
  expect_identical(cell_cols(c(10, NA, 3)), cell_limits(c(NA, 3), c(NA, 10)))
  expect_identical(cell_cols(4:16), cell_limits(c(NA, 4), c(NA, 16)))
  expect_error(cell_cols(c(7, 2)))

  expect_identical(cell_cols("B:D"), cell_limits(c(NA, 2), c(NA, 4)))
  expect_identical(cell_cols(c("C", "ZZ")), cell_limits(c(NA, 3), c(NA, 702)))
  expect_identical(cell_cols(c("C", NA)), cell_limits(c(NA, 3)))
  expect_error(cell_cols("Z:M"))
  expect_error(cell_cols(c("Z", "M")))

})

test_that("Print method works", {

  expect_output(print(cell_limits(c(NA, 7), c(3, NA))),
                "<cell_limits (1, 7) x (3, -)>", fixed = TRUE)
  expect_output(print(cell_limits(c(NA, 7), c(3, NA), "a sheet")),
                "<cell_limits (1, 7) x (3, -) in 'a sheet'>", fixed = TRUE)

})

test_that("dim method works", {

  expect_equivalent(dim(as.cell_limits("A1")), c(1, 1))
  expect_equivalent(dim(as.cell_limits("A1:F10")), c(10, 6))
  expect_equivalent(dim(cell_limits(c(1, 2), c(1, 5))), c(1, 4))
  expect_equivalent(dim(cell_limits(c(NA, 2), c(1, 5))), c(1, 4))
  expect_equivalent(dim(cell_limits(c(NA, 2), c(NA, 5))), c(NA_integer_, 4))
  expect_equivalent(dim(cell_limits(c(1, 1))), c(NA_integer_, NA_integer_))

})

test_that("Cell limits can be specified via anchor", {

  ## no input
  expect_identical(anchored(), as.cell_limits("A1"))
  expect_identical(anchored(anchor = "R4C2", dim = c(8, 2)),
                   cell_limits(c(4, 2), c(11, 3)))
  expect_identical(anchored(anchor = "A1", dim = c(3, 3), col_names = FALSE),
                   cell_limits(c(1, 1), c(3, 3)))
  expect_identical(anchored(anchor = "A1", dim = c(3, 3), col_names = TRUE),
                   cell_limits(c(1, 1), c(4, 3)))

  ## 2-dimensional input
  input <- head(iris)
  expect_identical(anchored(anchor = "R3C7", input = input),
                   cell_limits(c(3, 7), c(9, 11)))
  expect_identical(anchored(anchor = "R3C7", input = input, col_names = TRUE),
                   cell_limits(c(3, 7), c(9, 11)))
  expect_identical(anchored(anchor = "R3C7", input = input, col_names = FALSE),
                   cell_limits(c(3, 7), c(8, 11)))
  ## dim should have no effect here
  expect_identical(anchored(anchor = "R3C7", input = input, dim = c(2,2)),
                   cell_limits(c(3, 7), c(9, 11)))

  ## 1-dimensional input
  input <- LETTERS[1:8]
  expect_identical(anchored(anchor = "B5", input = input),
                   cell_limits(c(5, 2), c(12, 2)))
  expect_identical(anchored(anchor = "B5", input = input, byrow = TRUE),
                   cell_limits(c(5, 2), c(5, 9)))
  ## dim and col_names should have no effect here
  expect_identical(anchored(anchor = "B5", input = input, dim = c(5,5)),
                   cell_limits(c(5, 2), c(12, 2)))
  expect_identical(anchored(anchor = "B5", input = input, col_names = TRUE),
                   cell_limits(c(5, 2), c(12, 2)))

  expect_error(anchored(1))
  expect_warning(expect_error(anchored("A")))
  expect_error(anchored("A1:B10"))
  expect_error(anchored(dim = "eggplant"))
  expect_error(anchored(dim = 1:3))
  expect_error(anchored(input = head(iris, 2),
                        col_names = as.character(length(iris))))

})
