context("A1 <--> R1C1 conversion")

test_that("A1 format converts to R1C1 format", {
  expect_identical(A1_to_RC("$A$1"), "R1C1")
  expect_identical(A1_to_RC("$AB$10"), "R10C28")
  expect_identical(A1_to_RC(c("$A$1",  "$ZZ$100")),
                            c("R1C1", "R100C702"))
})

test_that("A1 relative and mixed references do not get converted", {
  expect_warning(x <- A1_to_RC("A1"))
  expect_identical(x, NA_character_)
  expect_warning(x <- A1_to_RC(c("A1", "A$1", "$A1", "$A$1")))
  expect_identical(x, c(NA_character_, NA_character_, NA_character_, "R1C1"))
})

test_that("strict = FALSE treats relative and mixed A1 references as absolute", {
  expect_identical(A1_to_RC("A1", strict = FALSE), "R1C1")
  expect_identical(A1_to_RC(c("A1", "A$1", "$A1", "$A$1"), strict = FALSE),
                   rep_len("R1C1", 4))
})

test_that("garbage alleged to be A1 formatted references is not converted", {
  expect_error(A1_to_RC(1:5))
  expect_error(A1_to_RC(factor(LETTERS)))
  expect_error(A1_to_RC("$Q$0"))
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
