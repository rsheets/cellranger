context("ra_ref class")

test_that("ra_ref constructor raises error for bad inputs", {
  expect_error(ra_ref(1:2))
  expect_error(ra_ref(row_abs = "A1"))
})

test_that("ra_ref constructor rejects absolute references < 1", {
expect_error(ra_ref(row_ref = -1))
  expect_error(ra_ref(col_ref = -2))
})

test_that("ra_ref constructor is not changing", {
  ra_list <- list(
    ra_ref(),
    ra_ref(2, TRUE,  3, FALSE),
    ra_ref(4, FALSE, 5,  TRUE),
    ra_ref(6, FALSE, 6, FALSE)
  )
  expect_equal_to_reference(ra_list, test_path("reference", "ra_list.rds"))
  ## THE FIRST TIME make sure wd is tests/testthat and do this:
  #expect_equal_to_reference(ra_list, file.path("reference", "ra_list.rds"))
})

test_that("ra_ref is converted to string", {
  expect_identical(to_string(ra_ref()), "R1C1")
  expect_identical(to_string(ra_ref(), fo = "A1"), "$A$1")
  expect_identical(to_string(ra_ref(2, TRUE,  3, FALSE)), "R2C[3]")
  expect_identical(to_string(ra_ref(4, FALSE, 5,  TRUE)), "R[4]C5")
  expect_identical(to_string(ra_ref(6, FALSE, -6, FALSE)), "R[6]C[-6]")
  ## special case when rel ref offset is 0 --> no square brackets
  expect_identical(to_string(ra_ref(0, FALSE)), "RC1")
  expect_identical(to_string(ra_ref(4, TRUE, 0, FALSE)), "R4C")
  expect_identical(to_string(ra_ref(0, FALSE, 0, FALSE)), "RC")
})

test_that("invalid single cell ref strings raise error", {
  expect_error(as.ra_ref("wtf huh?"))
  expect_error(as.ra_ref("A1:D4"))
})

test_that("file and sheet qualified cell ref strings raise warning", {
  expect_warning(as.ra_ref("Sheet1!$D$4"))
  expect_warning(as.ra_ref("[filename.xlsx]'a sheet'!R1C1"))
})

test_that("relative ra_refs become NA when converted to A1 formatted string", {
  expect_warning(
    expect_identical(to_string(ra_ref(2, TRUE, 3, FALSE), fo = "A1"),
                     NA_character_))
  expect_warning(
    expect_identical(to_string(ra_ref(2, FALSE, 3, TRUE), fo = "A1"),
                     NA_character_))
  expect_warning(
    expect_identical(to_string(ra_ref(-2, FALSE, 3, FALSE), fo = "A1"),
                     NA_character_))
})

test_that("list of ra_ref's is converted to character", {
  rar_list <-
    list(ra_ref(), ra_ref(2, TRUE, 5, TRUE), ra_ref(2, FALSE, 5, TRUE))
  expect_identical(to_string_v(rar_list), c("R1C1", "R2C5", "R[2]C5"))
  expect_warning(
    expect_identical(to_string_v(rar_list, fo = "A1"), c("$A$1", "$E$2", NA))
  )
})

test_that("convert an A1 formatted rel ref string to a ra_ref --> get NA", {
  expect_warning(expect_identical(as.ra_ref("A$4"),
                                  ra_ref(4, TRUE, NA, FALSE)))
  expect_warning(expect_identical(as.ra_ref("$A4"),
                                  ra_ref(NA, FALSE, 1, TRUE)))
  expect_warning(expect_identical(as.ra_ref("A4"),
                                  ra_ref(NA, FALSE, NA, FALSE)))
})

test_that("A1 formatted rel ref string --> absolutized ra_ref if strict = FALSE", {
  expect_identical(as.ra_ref("A4", strict = FALSE), ra_ref(4, TRUE, 1, TRUE))
})

test_that("valid cell ref string is converted to an ra_ref object", {
  expect_identical(as.ra_ref("R[1]C[-4]"), ra_ref(1, FALSE, -4, FALSE))
  expect_identical(as.ra_ref("$C$6"), ra_ref(6, TRUE, 3, TRUE))
  ## special case when rel ref offset is 0 --> no square brackets
  expect_warning(x <- as.ra_ref("RC1"))
  ## omfg RC1 is actually ambiguous
  expect_identical(x, ra_ref(0, FALSE, 1, TRUE))
  expect_identical(as.ra_ref("RC1", fo = "R1C1"), ra_ref(0, FALSE))
  expect_identical(as.ra_ref("R4C"), ra_ref(4, TRUE, 0, FALSE))
})

test_that("ra_ref --> string --> ra_ref round trips work", {
  roundtrip <- function(x, fo = NULL)
    expect_identical(x, to_string(as.ra_ref(x, fo), fo))
  roundtrip("R[1]C[-4]")
  roundtrip("RC1", "R1C1")
  roundtrip("R4C")
  roundtrip("R[-2]C8")
  ## strings in A1 format must contain absolute references
  roundtrip("$A$1", "A1")
})

test_that("vectorized version of as.ra_ref.character works", {
  input <- c("$A$1", "$F$14", "B$4", "D9")
  output <- list(ra_ref(), ra_ref(row_ref = 14, col_ref = 6),
                 ra_ref(4, col_ref = NA, col_abs = FALSE),
                 ra_ref(row_ref = 9, col_ref = 4))
  expect_identical(as.ra_ref_v(input, strict = FALSE, warn = FALSE), output)
})

test_that("vectorized version of as.ra_ref.cell_addr works", {
  input <- cell_addr(1:3, 1)
  output <- list(ra_ref(), ra_ref(row_ref = 2), ra_ref(row_ref = 3))
  expect_identical(as.ra_ref_v(input), output)
})
