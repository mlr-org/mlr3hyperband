context("TunerSuccessiveHalving")

test_that("TunerSuccessiveHalving", {
  test_tuner_successive_halving(n = 16, eta = 2, n_dim = 1)
  test_tuner_successive_halving(n = 16, eta = 2, n_dim = 2)
  test_tuner_successive_halving(n = 16, eta = 2, lower_bound = 2, n_dim = 1)
  test_tuner_successive_halving(n = 81, eta = 3, lower_bound = 1, upper_bound = 81, n_dim = 1)
})

