context("ra_ref class")

test_that("ra_ref constructor rejects input of wrong length or type", {
  expect_error(ra_ref(1:2))
  expect_error(ra_ref(rowAbs = "A1"))
  expect_error(ra_ref("A1"))
})

test_that("ra_ref constructor rejects absolute references < 1", {
  expect_error(ra_ref(-1))
  expect_error(ra_ref(colRef = -2))
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
})

test_that("relative references are not converted to A1 formatted strings", {
  expect_warning(ret <- to_string(ra_ref(2, TRUE,  3, FALSE), fo = "A1"))
  expect_identical(ret, NA_character_)
  expect_warning(ret <- to_string(ra_ref(-2, FALSE,  3, FALSE), fo = "A1"))
  expect_identical(ret, NA_character_)

})

test_that("invalid single cell ref strings raise error", {
  expect_error(as.ra_ref("wtf huh?"))
  expect_error(as.ra_ref("A1:D4"))
})

test_that("qualified cell ref strings raise warning", {
  expect_warning(as.ra_ref("Sheet1!$D4"))
  expect_warning(as.ra_ref("[filename.xlsx]'a sheet'!R[1]C[1]"))
})

test_that("ra_ref objects are made from cell ref strings", {
  expect_identical(as.ra_ref("A$4"), ra_ref(4, TRUE, 1, FALSE))
  expect_identical(as.ra_ref("R[1]C[-4]"), ra_ref(1, FALSE, -4, FALSE))
})