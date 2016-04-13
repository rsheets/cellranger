context("cell_addr class")

test_that("cell_addr constructor rejects input of wrong amount, length, type", {
  expect_error(cell_addr(1))
  expect_error(cell_addr(1:2, 1))
  expect_error(cell_addr(1, 1:2))
  expect_error(cell_addr("row!", "column!"))
})

test_that("cell_addr constructor rejects row, column < 1", {
  expect_error(cell_addr(1, -1))
  expect_error(cell_addr(-1, 1))
})

test_that("cell_addr constructor works", {
  ca <- cell_addr(3, 7)
  expect_is(ca, "cell_addr")
  expect_identical(ca["row"], c(row = 3L))
  expect_identical(ca["col"], c(col = 7L))
})

test_that("cell_addr objects can be converted to ra_ref", {
  expect_identical(as.ra_ref(cell_addr(2, 5)),
                   ra_ref(rowRef = 2, colRef = 5))
})

test_that("cell_addr objects can be converted to string", {
  expect_identical(to_string(cell_addr(3, 8)), "R3C8")
  expect_identical(to_string(cell_addr(3, 8), fo = "A1"), "$H$3")
})

test_that("valid ra_ref objects can be converted to cell_addr", {
  expect_identical(as.cell_addr(ra_ref()), cell_addr(1, 1))
  expect_identical(as.cell_addr(ra_ref(2, TRUE, 5, TRUE)), cell_addr(2, 5))
})

test_that("we refuse to convert invalid ra_ref objects to cell_addr", {
  expect_error(as.cell_addr(ra_ref(2, FALSE, 5, FALSE)))
  expect_error(as.cell_addr(ra_ref(2, TRUE, 5, FALSE)))
  expect_error(as.cell_addr(ra_ref(2, FALSE, 5, TRUE)))
})

test_that("valid cell ref strings can be converted to cell_addr", {
  expect_identical(as.cell_addr("$D$12"), cell_addr(12, 4))
  expect_identical(as.cell_addr("R4C3"), cell_addr(4, 3))
})

test_that("we refuse to convert invalid cell ref strings to cell_addr", {
  expect_error(as.cell_addr("$F2"))
  expect_error(as.cell_addr("F$2"))
  expect_error(as.cell_addr("R[-4]C3"))
  expect_error(as.cell_addr("R4C[3]"))
  expect_error(as.cell_addr("RC"))
  expect_error(as.cell_addr("RC3"))
})
