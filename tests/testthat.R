if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(checkmate)
  library(mlr3hyperband)

  test_check("mlr3hyperband")
}
