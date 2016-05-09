context("cell_addr class")

test_that("cell_addr constructor requires row and col and checks lengths", {
  expect_error(cell_addr(1))
  expect_error(cell_addr(1:2, 1:3))
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

test_that("cell_addr constructor accepts NAs (and is not picky about type)", {
  ca <- cell_addr(NA, NA)
  expect_is(ca, "cell_addr")
  expect_identical(ca$row, NA_integer_)
  expect_identical(ca$col, NA_integer_)
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
  expect_identical(addr_row(ca), 1:3)
  expect_identical(addr_col(ca), rep_len(6L, 3))
})

test_that("cell_addr objects can be converted to ra_ref", {
  expect_identical(as.ra_ref(cell_addr(2, 5)),
                   ra_ref(row_ref = 2, col_ref = 5))
})

test_that("cell_addr objects can be converted to string", {
  expect_identical(to_string(cell_addr(3, 8)), "R3C8")
  expect_identical(to_string(cell_addr(3, 8), fo = "A1"), "$H$3")
  expect_identical(to_string(cell_addr(3, 8), fo = "A1", strict = FALSE), "H3")
})

test_that("valid ra_ref objects can be converted to cell_addr", {
  expect_identical(as.cell_addr(ra_ref()), cell_addr(1, 1))
  expect_identical(as.cell_addr(ra_ref(2, TRUE, 5, TRUE)), cell_addr(2, 5))
})

test_that("ra_ref objects w/ NAs become cell_addr objects with NAs", {
  expect_warning(expect_identical(as.cell_addr(ra_ref(2, FALSE, 5, FALSE)),
                                  cell_addr(NA, NA)))
  expect_warning(expect_identical(as.cell_addr(ra_ref(2, TRUE, 5, FALSE)),
                                  cell_addr(2, NA)))
  expect_warning(expect_identical(as.cell_addr(ra_ref(2, FALSE, 5, TRUE)),
                                  cell_addr(NA, 5)))
})

test_that("valid cell ref strings can be converted to cell_addr", {
  expect_identical(as.cell_addr("$D$12"), cell_addr(12, 4))
  expect_identical(as.cell_addr("R4C3"), cell_addr(4, 3))
})

test_that("relative refs in cell ref strings create NAs in cell_addr", {
  expect_warning(expect_identical(as.cell_addr("$F2"), cell_addr(NA, 6)))
  expect_warning(expect_identical(as.cell_addr("F$2"), cell_addr(2, NA)))
  expect_warning(expect_identical(as.cell_addr("R4C[3]"), cell_addr(4, NA)))
  expect_warning(expect_identical(as.cell_addr("RC"), cell_addr(NA, NA)))
})

test_that("relative cell ref strings convert to cell_addr if strict = FALSE", {
  expect_identical(as.cell_addr("F2", strict = FALSE), as.cell_addr("$F$2"))
})

test_that("a vector of cell ref strings is converted to cell_addr", {
  x <- as.cell_addr(c("$D$12", "$C$4"))
  expect_identical(x, cell_addr(row = c(12L, 4L), col = c(4L, 3L)))
  y <- lapply(c("$D$12", "$C$4"), as.cell_addr)
  ## strip cell_addr class so its indexing methods don't mess with transpose
  y <- lapply(y, unclass)
  expect_equivalent(x, transpose(y))
})
