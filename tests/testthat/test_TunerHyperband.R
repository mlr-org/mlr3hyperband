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
  tuner = TunerHyperband$new(eta = 2L)
  tuner$tune(inst)

  results = inst$archive()[, .(frac = sapply(params, "[", "subsample.frac"), cp = sapply(params, "[", "classif.rpart.cp"), minsplit = sapply(params, "[", "classif.rpart.minsplit"), classif.ce)]

  expect_data_table(results, ncols = 4, nrows = 35)
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
