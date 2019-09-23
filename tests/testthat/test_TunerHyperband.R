context("TunerHyperband")


test_that("TunerHyperband singlecrit", {

  test_tuner("hyperband", eta = 3L, lower_b = 1, upper_b = 81, real_evals = 81)
  test_tuner("hyperband", eta = 2L, lower_b = 1, upper_b = 16, real_evals = 16, n_dim = 2L)
  test_tuner_dependencies("hyperband", eta = 3L, lower_b = 1, upper_b = 81, term_evals = 81)

})


test_that("TunerHyperband multicrit", {

  test_tuner("hyperband", eta = 3L, lower_b = 1, upper_b = 81, real_evals = 81, measures = c("classif.fpr", "classif.tpr"))
  test_tuner("hyperband", eta = 2L, lower_b = 1, upper_b = 16, real_evals = 16, n_dim = 2L, measures = c("classif.fpr", "classif.tpr"))

})
