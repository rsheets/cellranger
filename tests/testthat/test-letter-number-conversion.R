context("letter <--> number conversion")

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
