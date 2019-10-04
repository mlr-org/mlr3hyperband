context("TunerHyperband")

library(mlr3learners)
library(mlr3pipelines)


test_that("TunerHyperband singlecrit", {

  test_tuner("hyperband", eta = 3L, lower_b = 1, upper_b = 81)
  test_tuner("hyperband", eta = 2L, lower_b = 1, upper_b = 16, term_evals = 10, n_dim = 2L)
  test_tuner_dependencies("hyperband", eta = 3L, lower_b = 1, upper_b = 81)

})


test_that("TunerHyperband multicrit", {

  test_tuner("hyperband", eta = 3L, lower_b = 1, upper_b = 81, measures = c("classif.fpr", "classif.tpr"))
  test_tuner("hyperband", eta = 2L, lower_b = 1, upper_b = 16, term_evals = 10, n_dim = 2L, measures = c("classif.fpr", "classif.tpr"))

})


test_that("TunerHyperband using subsampling", {

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

  results = inst$archive()[, .(cp = sapply(params, "[", "cp"), minsplit = sapply(params, "[", "minsplit"), classif.ce)]

  expect_data_table(results, ncols = 3, nrows = 35)
})


test_that("TunerHyperband using custom sampler", {

  library(mlr3learners)
  set.seed(123)

  # define hyperparameter and budget parameter for tuning with hyperband
  params = list(
    ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
    ParamDbl$new("eta",     lower = 0, upper = 1),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  )


  inst = TuningInstance$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    term("evals", n_evals = 100000)
  )

  # create custom sampler:
  # - beta distribution with alpha = 2 and beta = 5
  # - categorical distribution with custom probabilities
  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
    Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  tuner = TunerHyperband$new(eta = 2L, sampler = sampler)
  tuner$tune(inst)

  results = inst$archive()[, .(
    nrounds = sapply(params, "[", "nrounds"), 
    eta = sapply(params, "[", "eta"), 
    booster = sapply(params, "[", "booster"), 
    classif.ce
  )]

  expect_data_table(results, ncols = 4, nrows = 72)
})
