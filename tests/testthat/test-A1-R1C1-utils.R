context("A1, R1C1 detection and parsing")

A1 <- c("A1", "$A1", "A$1", "$A$1", "a1")
R1C1 <- c("R1C1", "R1C[-1]", "R[-1]C1", "R[-1]C[9]")

test_that("A1 refs are detected as such and R1C1 are not", {
  expect_true(all(is_A1(A1)))
  expect_false(any(is_R1C1(A1)))
})

test_that("R1C1 refs are detected as such and A1 are not", {
  expect_true(all(is_R1C1(R1C1)))
  expect_false(all(is_A1(R1C1)))
})

test_that("ambiguous refs are detected as both A1 and R1C1", {
  expect_true(is_A1("RC3"))
  expect_true(is_R1C1("RC3"))
})

