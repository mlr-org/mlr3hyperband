library(mlr3learners)
library(mlr3pipelines)

context("TunerHyperband")


test_that("TunerHyperband singlecrit", {

  set.seed(1234)
  test_tuner("hyperband", eta = 3L, lower_b = 1, upper_b = 27)
  test_tuner("hyperband", eta = 2L, lower_b = 1, upper_b = 8, term_evals = 10, n_dim = 2L)
  test_tuner_dependencies("hyperband", eta = 3L, lower_b = 1, upper_b = 27)

})


test_that("TunerHyperband multicrit", {

  set.seed(1234)
  test_tuner("hyperband", eta = 3L, lower_b = 1, upper_b = 27, measures = c("classif.fpr", "classif.tpr"))
  test_tuner("hyperband", eta = 2L, lower_b = 1, upper_b = 8, term_evals = 10, n_dim = 2L, measures = c("classif.fpr", "classif.tpr"))

})


test_that("TunerHyperband using CV", {

  set.seed(123)
   
  # define hyperparameter and budget parameter for tuning with hyperband
  ps = ParamSet$new(list(

    ParamInt$new("nrounds", lower = 1, upper = 8, tags = "budget"),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  ))

  # tuning instance with 2-fold CV
  inst = TuningInstance$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("cv", folds = 2),
    msr("classif.ce"),
    ps,
    term("evals", n_evals = 100000)
  )

  # hyperband + tuning
  tuner = TunerHyperband$new(eta = 2L)
  expect_tuner(tuner)
  tuner$tune(inst)

  results = inst$archive()[, .(
    nrounds = sapply(params, "[", "nrounds"),
    booster = sapply(params, "[", "booster"),
    classif.ce
  )]

  expect_data_table(results, ncols = 3, nrows = 35)
})


test_that("TunerHyperband using subsampling", {

  set.seed(123)
   
  # define Graph Learner from rpart with subsampling as preprocessing step
  pops = mlr_pipeops$get("subsample")
  graph_learner = GraphLearner$new(pops %>>% lrn("classif.rpart"))
   
  # define with extended hyperparameters with subsampling fraction as budget
  # ==> no learner budget is required
  params = list(
    ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
    ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
    ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
  )
   
  # define TuningInstance with the Graph Learner and the extended hyperparams
  inst = TuningInstance$new(
    tsk("iris"),
    graph_learner,
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    term("evals", n_evals = 100000)
  )
   
  # define and call hyperband as usual
  tuner = TunerHyperband$new(eta = 4L)
  expect_tuner(tuner)
  tuner$tune(inst)

  results = inst$archive()[, .(frac = sapply(params, "[", "subsample.frac"), cp = sapply(params, "[", "classif.rpart.cp"), minsplit = sapply(params, "[", "classif.rpart.minsplit"), classif.ce)]

  expect_data_table(results, ncols = 4, nrows = 7)
})


test_that("TunerHyperband using subsampling and non-integer eta", {

  set.seed(123)
   
  # define Graph Learner from rpart with subsampling as preprocessing step
  pops = po("subsample")
  graph_learner = pops %>>% lrn("classif.rpart")
   
  # define with extended hyperparameters with subsampling fraction as budget
  # ==> no learner budget is required
  params = list(
    ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
    ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
    ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
  )
   
  # define TuningInstance with the Graph Learner and the extended hyperparams
  inst = TuningInstance$new(
    tsk("iris"),
    graph_learner,
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    term("evals", n_evals = 100000)
  )
   
  # define and call hyperband as usual
  tuner = TunerHyperband$new(eta = 3.5)
  expect_tuner(tuner)
  tuner$tune(inst)

  results = inst$archive()[, .(
    frac = sapply(params, "[", "subsample.frac"),
    cp = sapply(params, "[", "classif.rpart.cp"), 
    minsplit = sapply(params, "[", "classif.rpart.minsplit"),
    classif.ce
  )]

  expect_data_table(results, ncols = 4, nrows = 7)
})


test_that("TunerHyperband using param trafo and non-integer eta", {

  set.seed(123)
   
  # define hyperparameter and budget parameter for tuning with hyperband
  ps = ParamSet$new(list(
    ParamInt$new("nrounds", lower = 1, upper = 10, tags = "budget"),
    ParamDbl$new("eta",     lower = 0, upper = 1),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  ))

  #ps$trafo = function(x, param_set) {
  #  x$nrounds = round(x$nrounds)
  #  return(x)
  #}

  inst = TuningInstance$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("holdout"),
    msr("classif.ce"),
    ps,
    term("evals", n_evals = 100000)
  )

  # hyperband + tuning
  tuner = TunerHyperband$new(eta = 3.9)
  expect_tuner(tuner)
  tuner$tune(inst)

  results = inst$archive()[, .(
    nrounds = sapply(params, "[", "nrounds"),
    eta     = sapply(params, "[", "eta"),
    booster = sapply(params, "[", "booster"),
    classif.ce
  )]

  expect_data_table(results, ncols = 4, nrows = 7)
})


test_that("TunerHyperband using custom sampler", {

  set.seed(123)

  # define hyperparameter and budget parameter for tuning with hyperband
  params = list(
    ParamInt$new("nrounds", lower = 1, upper = 8, tags = "budget"),
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
  # not enough params defined
  sampler_fail1 = SamplerJointIndep$new(list(
    Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
  ))
  # budget param defined
  sampler_fail2 = SamplerJointIndep$new(list(
    Sampler1D$new(params[[1]]),
    Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
    Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  # check if asserts throw errors on false samplers
  tuner = TunerHyperband$new(eta = 2L, sampler = sampler_fail1)
  expect_tuner(tuner)
  expect_error(tuner$tune(inst), "Assertion on ")
  tuner = TunerHyperband$new(eta = 2L, sampler = sampler_fail2)
  expect_tuner(tuner)
  expect_error(tuner$tune(inst), "Assertion on ")

  # check if correct sampler works
  tuner = TunerHyperband$new(eta = 2L, sampler = sampler)
  expect_tuner(tuner)
  tuner$tune(inst)

  results = inst$archive()[, .(
    nrounds = sapply(params, "[", "nrounds"), 
    eta = sapply(params, "[", "eta"), 
    booster = sapply(params, "[", "booster"), 
    classif.ce
  )]

  expect_data_table(results, ncols = 4, nrows = 35)
})
