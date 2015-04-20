context("cell specification")

test_that("Column letter converts to correct column number", {

  expect_equal(letter_to_num("A"), 1)
  expect_equal(letter_to_num("AB"), 28)
  expect_equal(letter_to_num(c("A", "AH", "ABD", "XFD")), c(1, 34, 732, 16384))

})

test_that("Column number converts to correct column letter", {

  expect_equal(num_to_letter(1), "A")
  expect_equal(num_to_letter(28), "AB")
  expect_equal(num_to_letter(c(1, 34, 732, 16384)), c("A", "AH", "ABD", "XFD"))

})

test_that("A1 notation converts to R1C1 notation", {

  expect_equal(A1_to_RC("A1"), "R1C1")
  expect_equal(A1_to_RC("AB10"), "R10C28")
  expect_equal(A1_to_RC(c("A1", "ZZ100", "ZZZ15")),
               c("R1C1", "R100C702", "R15C18278"))

})

test_that("R1C1 notation converts to A1 notation", {

  expect_equal(RC_to_A1("R1C1"), "A1")
  expect_equal(RC_to_A1("R10C28"), "AB10")
  expect_equal(RC_to_A1(c("R1C1", "R100C702", "R15C18278")),
               c("A1", "ZZ100", "ZZZ15"))

})

test_that("Cell range is converted to a cell_limit object and vice versa", {

  rgA1 <- "A1:C4"
  rgRC <- "R1C1:R4C3"
  rgCL <- structure(list(rows = c(1, 4), cols = c(1, 3)), class = "cell_limits")
  expect_equal(as.cell_limits(rgA1), rgCL)
  expect_equal(as.cell_limits(rgRC), rgCL)
  expect_equal(convert_cell_limits_to_range(rgCL), rgA1)
  expect_equal(convert_cell_limits_to_range(rgCL, RC = TRUE), rgRC)

  rgA1 <- "E7"
  rgA1A1 <- "E7:E7"
  rgRC <- "R7C5"
  rgRCRC <- "R7C5:R7C5"
  rgCL <-
    structure(list(rows = c(7, 7), cols = c(5, 5)), class = "cell_limits")
  expect_equal(as.cell_limits(rgA1), rgCL)
  expect_equal(as.cell_limits(rgRC), rgCL)
  expect_equal(as.cell_limits(rgA1A1), rgCL)
  expect_equal(as.cell_limits(rgRCRC), rgCL)
  expect_equal(convert_cell_limits_to_range(rgCL), rgA1A1)
  expect_equal(convert_cell_limits_to_range(rgCL, RC = TRUE), rgRCRC)

})

test_that("Bad cell ranges throw errors", {

  expect_error(as.cell_limits("eggplant"))
  expect_error(as.cell_limits("A:B10"))
  expect_error(as.cell_limits("A1:R3C3"))
  expect_error(as.cell_limits("A1:B2:C3"))
  expect_error(as.cell_limits("14:17"))
  expect_error(as.cell_limits(14:17))
  expect_error(as.cell_limits(B2:D9))
  expect_error(cell_limits(rows = c(-1, 3), cols = c(1, 4)))
  expect_error(cell_limits(rows = c(0, 3), cols = c(1, 4)))
  expect_error(cell_limits(rows = c(1, 3), cols = c(4, 1)))

})
