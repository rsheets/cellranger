context("A1 <--> R1C1 conversion")

test_that("A1 format converts to R1C1 format", {
  expect_identical(A1_to_R1C1("$AB$10"), "R10C28")
  expect_identical(A1_to_R1C1(c("$A$1",  "$ZZ$100")),
                            c("R1C1", "R100C702"))
})

test_that("A1 relative and mixed references do not get converted", {
  expect_warning(x <- A1_to_R1C1("A1"))
  expect_identical(x, NA_character_)
  expect_warning(x <- A1_to_R1C1(c("A1", "A$1", "$A1", "$A$1")))
  expect_identical(x, c(NA_character_, NA_character_, NA_character_, "R1C1"))
})

test_that("strict = FALSE treats relative and mixed A1 references as absolute", {
  expect_identical(A1_to_R1C1("A1", strict = FALSE), "R1C1")
  expect_warning(
    expect_identical(
      A1_to_R1C1(c("A1", "A$1", "$A1", "$A$1"), strict = FALSE),
      c("R1C1", NA, NA, "R1C1"))
  )
})

test_that("R1C1 notation converts to A1 notation", {
  expect_identical(R1C1_to_A1("R10C28"), "$AB$10")
  expect_identical(R1C1_to_A1("R10C28", strict = FALSE), "AB10")
  expect_identical(R1C1_to_A1(c("R1C1", "R100C702")), c("$A$1",  "$ZZ$100"))
})

test_that("R1C1 relative and mixed references do not get converted", {
  expect_warning(x <- R1C1_to_A1("RC"))
  expect_identical(x, NA_character_)
  expect_warning(x <- R1C1_to_A1(c("R[1]C[1]", "RC[1]", "R[-2]C")))
  expect_identical(x, rep_len(NA_character_, 3))
})

test_that("invalid input is not converted A1 to R1C1", {
  expect_error(A1_to_R1C1(1:5))
  expect_error(A1_to_R1C1(factor(LETTERS)))
})

test_that("invalid input is not converted R1C1 to A1", {
  expect_error(R1C1_to_A1(1:5))
  expect_error(R1C1_to_A1(factor(LETTERS)))
})


