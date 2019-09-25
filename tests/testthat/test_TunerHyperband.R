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


test_that("TunerHyperband using subsampling", {

  library(mlr3pipelines)
  set.seed(123)

  # define hyperparameter for tuning with hyperband
  params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  )


  inst = TuningInstance$new(
    tsk("iris"),
    lrn("classif.rpart"),
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    term("evals", n_evals = 100000)
  )


  tuner = TunerHyperband$new(eta = 2L, use_subsamp = TRUE)
  tuner$tune(inst)

  results = inst$archive()[, c("cp", "minsplit", "classif.ce")]

  expect_data_table(results, ncols = 3, nrows = 35)
})
