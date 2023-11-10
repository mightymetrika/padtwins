test_that("Test that A works with an anonymus function", {
  set.seed(110)
  generate_A <- function() matrix(rhyper(4, 4, 5, 3), nrow = 2, ncol = 2)
  padt <- padtwins(generate_A)
  expect_s3_class(padt, "padtwins")
})


test_that("Test that A works with a preset matrix", {
  set.seed(102)
  A <- matrix(c(1, 2, -2, 1), nrow = 2, ncol = 2)
  padt <- padtwins(A=A)
  expect_s3_class(padt, "padtwins")
})

