context("cell_addr class")

test_that("cell_addr constructor rejects input of wrong amount, length, type", {
  expect_error(cell_addr(1))
  expect_error(cell_addr(1:2, 1:3))
  expect_error(cell_addr("row!", "column!"))
})

test_that("cell_addr constructor rejects row, column < 1", {
  expect_error(cell_addr(1, -1))
  expect_error(cell_addr(-1, 1))
})

test_that("cell_addr constructor works for single cell", {
  ca <- cell_addr(3, 7)
  expect_is(ca, "cell_addr")
  expect_identical(ca$row, 3L)
  expect_identical(ca$col, 7L)
})

test_that("cell_addr constructor works for multiple cells", {
  ca <- cell_addr(c(3, 10), c(7, 2))
  expect_is(ca, "cell_addr")
  expect_identical(ca$row, c(3L, 10L))
  expect_identical(ca$col, c(7L, 2L))
})

test_that("cell_addr constructor recycles length 1 row or col", {
  ca <- cell_addr(1:3, 6)
  expect_is(ca, "cell_addr")
  expect_identical(ca$row, 1:3)
  expect_identical(ca$col, rep_len(6L, 3))
})

test_that("cell_addr `[` indexing works", {
  ca <- cell_addr(1:3, 6)
  expect_identical(ca[c(1, 3)], cell_addr(c(1L, 3L), 6L))
  expect_identical(ca[-2], cell_addr(c(1L, 3L), 6L))
  expect_identical(ca[c(TRUE, FALSE, TRUE)], cell_addr(c(1L, 3L), 6L))
})

test_that("cell_addr `[[` indexing works", {
  ca <- cell_addr(1:3, 6)
  expect_identical(ca[[3]], cell_addr(3L, 6L))
  expect_error(ca[[-2]])
})

test_that("cell_addr length method works", {
  ca <- cell_addr(1:3, 6)
  expect_length(ca, 3L)
})

test_that("row and column extraction work for cell_addr objects", {
  ca <- cell_addr(1:3, 6)
  expect_identical(cell_row(ca), 1:3)
  expect_identical(cell_col(ca), rep_len(6L, 3))
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

test_that("a vector of cell ref strings is converted to cell_addr", {
  x <- as.cell_addr(c("$D$12", "$C$4"))
  expect_identical(x, cell_addr(row = c(12L, 4L), col = c(4L, 3L)))
  y <- lapply(c("$D$12", "$C$4"), as.cell_addr)
  ## strip cell_addr class so its indexing methods don't mess with transpose
  y <- lapply(y, unclass)
  expect_equivalent(x, transpose(y))
})
