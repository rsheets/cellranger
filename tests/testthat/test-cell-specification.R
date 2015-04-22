context("cell specification")

test_that("Dollar signs are removed", {

  expect_equal(rm_dollar_signs(c("A$1:$B$32", "$D11")), c("A1:B32", "D11"))
  expect_equal(as.cell_limits("A$1:$B$32"), cell_limits(c(1, 32), c(1, 2)))

})

test_that("Column letter converts to correct column number", {

  expect_equal(letter_to_num("A"), 1L)
  expect_equal(letter_to_num("AB"), 28L)
  expect_equal(letter_to_num(
    c("A", "AH", NA, "", "ABD",  "XFD")),
    c( 1L,  34L, NA, NA,  732L, 16384L))

  expect_error(letter_to_num(1:5))
  expect_error(letter_to_num(factor(LETTERS)))

})

test_that("Column number converts to correct column letter", {

  expect_equal(num_to_letter(1), "A")
  expect_equal(num_to_letter(28), "AB")
  expect_equal(num_to_letter(
    c(  34, NA,  0, -4,  732,  16384, 4.8)),
    c("AH", NA, NA, NA, "ABD", "XFD", "D"))

  expect_error(num_to_letter("hi there"))
  expect_error(num_to_letter("100"))

})

test_that("A1 notation converts to R1C1 notation", {

  expect_equal(A1_to_RC("A1"), "R1C1")
  expect_equal(A1_to_RC("AB10"), "R10C28")
  expect_equal(A1_to_RC(
    c(  "A1",    "ZZ100", NA, "Q", "4", "",     "ZZZ15", "Q0")),
    c("R1C1", "R100C702", NA,  NA,  NA, NA, "R15C18278",   NA))

  expect_error(A1_to_RC(1:5))
  expect_error(A1_to_RC(factor(LETTERS)))

})

test_that("R1C1 notation converts to A1 notation", {

  expect_equal(RC_to_A1("R1C1"), "A1")
  expect_equal(RC_to_A1("R10C28"), "AB10")
  expect_equal(RC_to_A1(
    c("R1C1", "R100C702", "R15C18278", "", NA, "R5C0")),
    c(  "A1",    "ZZ100",     "ZZZ15", NA, NA,     NA))

  expect_error(RC_to_A1(1:5))
  expect_error(RC_to_A1(factor(LETTERS)))

})

test_that("Cell range is converted to a cell_limit object and vice versa", {

  rgA1 <- "A1:C4"
  rgRC <- "R1C1:R4C3"
  rgCL <- cell_limits(rows = c(1, 4), cols = c(1, 3))
  expect_equal(as.cell_limits(rgA1), rgCL)
  expect_equal(as.cell_limits(rgRC), rgCL)
  expect_equal(as.range(rgCL), rgA1)
  expect_equal(as.range(rgCL, RC = TRUE), rgRC)

  rgA1 <- "E7"
  rgA1A1 <- "E7:E7"
  rgRC <- "R7C5"
  rgRCRC <- "R7C5:R7C5"
  rgCL <- cell_limits(rows = c(7, 7), cols = c(5, 5))
  expect_equal(as.cell_limits(rgA1), rgCL)
  expect_equal(as.cell_limits(rgRC), rgCL)
  expect_equal(as.cell_limits(rgA1A1), rgCL)
  expect_equal(as.cell_limits(rgRCRC), rgCL)
  expect_equal(as.range(rgCL), rgA1A1)
  expect_equal(as.range(rgCL, RC = TRUE), rgRCRC)

  rgCL <- cell_limits(rows = c(NA, 4), cols = c(1, NA))
  expect_true(is.na(as.range(rgCL)))

})

test_that("Bad cell ranges throw errors", {

  expect_error(as.cell_limits("eggplant"))
  expect_error(as.cell_limits("A:B10"))
  expect_error(as.cell_limits(":B10"))
  expect_error(as.cell_limits("A1:R3C3"))
  expect_error(as.cell_limits("A1:B2:C3"))
  expect_error(as.cell_limits("14:17"))
  expect_error(as.cell_limits(14:17))
  expect_error(as.cell_limits(B2:D9))
  expect_error(cell_limits(rows = c(-1, 3), cols = c(1, 4)))
  expect_error(cell_limits(rows = c(0, 3), cols = c(1, 4)))
  expect_error(cell_limits(rows = c(1, 3), cols = c(4, 1)))

})

test_that("Degenerate, all-NA input is tolerated", {


  cl <- cell_limits()
  expect_is(cl, "cell_limits")
  expect_is(cl$rows, "integer")

  cl2 <- cell_limits(c(NA, NA))
  expect_identical(cl, cl2)

  cl3 <- cell_limits(cols = c(NA, NA))
  expect_identical(cl, cl3)

})

test_that("Row-only specifications work", {

  expect_identical(cell_rows(c(NA, NA)), cell_limits())
  expect_identical(cell_rows(c(NA, 3)), cell_limits(rows = c(NA, 3)))
  expect_identical(cell_rows(c(7, NA)), cell_limits(rows = c(7, NA)))
  expect_identical(cell_rows(c(3, NA, 10)), cell_limits(rows = c(3, 10)))
  expect_identical(cell_rows(c(10, NA, 3)), cell_limits(rows = c(3, 10)))
  expect_identical(cell_rows(4:16), cell_limits(rows = c(4L, 16L)))
  expect_error(cell_rows(c(7, 2)))

})

test_that("Column-only specifications work", {

  expect_identical(cell_cols(c(NA, NA)), cell_limits())
  expect_identical(cell_cols(c(NA, 3)), cell_limits(cols = c(NA, 3)))
  expect_identical(cell_cols(c(7, NA)), cell_limits(cols = c(7, NA)))
  expect_identical(cell_cols(c(3, NA, 10)), cell_limits(cols = c(3, 10)))
  expect_identical(cell_cols(c(10, NA, 3)), cell_limits(cols = c(3, 10)))
  expect_identical(cell_cols(4:16), cell_limits(cols = c(4L, 16L)))
  expect_error(cell_cols(c(7, 2)))

  expect_identical(cell_cols("B:D"), cell_limits(cols = c(2L, 4L)))
  expect_identical(cell_cols(c("C", "ZZ")), cell_limits(cols = c(3L, 702L)))
  expect_identical(cell_cols(c("C", NA)), cell_limits(cols = c(3L, NA)))
  expect_error(cell_cols("Z:M"))
  expect_error(cell_cols(c("Z", "M")))

})

test_that("Print method works", {

  expect_output(cell_limits(c(NA, 7), c(3, NA)),
                "<cell_limits (-, 3) x (7, -)>", fixed = TRUE)

})

test_that("dim method works", {

  expect_equivalent(dim(as.cell_limits("A1")), c(1, 1))
  expect_equivalent(dim(as.cell_limits("A1:F10")), c(10, 6))
  expect_equivalent(dim(cell_limits(c(1, 1), c(2, 5))), c(1, 4))
  expect_equivalent(dim(cell_limits(c(NA, 1), c(2, 5))), c(NA_integer_, 4))
  expect_equivalent(dim(cell_limits(c(NA, NA), c(2, 5))), c(NA_integer_, 4))
  expect_equivalent(dim(cell_limits(c(NA, 1), c(NA, 5))),
                    c(NA_integer_, NA_integer_))

})

test_that("Cell limits can be specified via anchor", {

  expect_identical(anchored(), as.cell_limits("A1"))
  expect_identical(anchored(anchor = "R4C2", dim = c(8, 2)),
                   cell_limits(c(4, 11), c(2, 3)))

  input <- head(iris)
  expect_identical(anchored(anchor = "R3C7", input = input),
                   cell_limits(c(3, 9), c(7, 11)))
  expect_identical(anchored(anchor = "R3C7", input = input, header = FALSE),
                   cell_limits(c(3, 8), c(7, 11)))

  input <- LETTERS[1:8]
  expect_identical(anchored(anchor = "B5", input = input),
                   cell_limits(c(5, 12), c(2, 2)))
  expect_identical(anchored(anchor = "B5", input = input, byrow = TRUE),
                   cell_limits(c(5, 5), c(2, 9)))

  expect_error(anchored(1))
  expect_error(anchored("A"))
  expect_error(anchored("A1:B10"))

})
