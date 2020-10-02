test_that("TunerSuccessiveHalving", {
  test_tuner_successive_halving(n = 16, eta = 2, n_dim = 1)
  test_tuner_successive_halving(n = 16, eta = 2, n_dim = 2)
})

