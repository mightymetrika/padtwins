test_that("quick_sim works when A is a matrix", {

  set.seed(102)
  A <- matrix(c(1, 2, -2, 1), nrow = 2, ncol = 2)
  qs <- quick_sim(padtwins_func = function()padtwins(A=A,
                                                     generate_P = function(A) matrix(stats::rgeom(n = nrow(A) * ncol(A), prob = 0.5), nrow = nrow(A))),
                  iterations = 100)
  expect_true(class(qs) == "quick_sim_result")
  })

test_that("quick_sim works when A is a function", {

  set.seed(110)
  generate_A <- function() matrix(rgeom(4, 0.75), nrow = 2, ncol = 2)
  qs <- quick_sim(padtwins_func = function()padtwins(A=generate_A,
                                                     generate_P = function(A) matrix(stats::rgeom(n = nrow(A) * ncol(A), prob = 0.5), nrow = nrow(A))),
                  iterations = 100)
  expect_true(class(qs) == "quick_sim_result")
})
