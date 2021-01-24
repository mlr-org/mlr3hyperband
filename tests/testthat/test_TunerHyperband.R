test_that("TunerHyperband works with TuningInstanceSingleCrit", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  test_tuner_hyperband(eta = 3L, lower_budget = 1, upper_budget = 27)
  test_tuner_hyperband(eta = 2L, lower_budget = 1, upper_budget = 8, term_evals = 10, n_dim = 2L)
  test_tuner_hyperband_dependencies(eta = 3L, lower_budget = 1, upper_budget = 27)
})


test_that("TunerHyperband works with TuningInstanceMultiCrit", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  test_tuner_hyperband(eta = 3L, lower_budget = 1, upper_budget = 27, measures = c("classif.fpr", "classif.tpr"))
  test_tuner_hyperband(eta = 2L, lower_budget = 1, upper_budget = 8, term_evals = 10, n_dim = 2L, measures = c("classif.fpr", "classif.tpr"))
})

test_that("TunerHyperband works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  # define Graph Learner from rpart with subsampling as preprocessing step
  pops = po("subsample")
  graph_learner = pops %>>% lrn("classif.rpart")

  # define with extended hyperparameters with subsampling fraction as budget
  # ==> no learner budget is required
  search_space = ps(
    classif.rpart.cp = p_dbl(lower = 0.001, upper = 0.1),
    classif.rpart.minsplit = p_int(lower = 1, upper = 10),
    subsample.frac = p_dbl(lower = 0.1, upper = 1, tags = "budget")
  )

  # define TuningInstanceSingleCrit with the Graph Learner and the extended hyperparams
  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = graph_learner, resampling = rsmp("holdout"),
    measure = msr("classif.ce"), terminator = trm("evals", n_evals = 100000), search_space = search_space
  )

  tuner = tnr("hyperband", eta = 3.5)
  expect_data_table(tuner$optimize(instance))
})

test_that("TunerHyperband works with custom sampler", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  search_space = ps(
    nrounds = p_int(lower = 1, upper = 8, tags = "budget"),
    eta = p_dbl(lower = 0, upper = 1),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
  )

  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = lrn("classif.xgboost"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("none"),
    search_space = search_space)

  # create custom sampler:
  # - beta distribution with alpha = 2 and beta = 5
  # - categorical distribution with custom probabilities
  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(search_space$params[[2]], function(n) rbeta(n, 2, 5)),
    Sampler1DCateg$new(search_space$params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  tuner = tnr("hyperband", eta = 2L, sampler = sampler)
  expect_data_table(tuner$optimize(instance))

  # not enough params defined
  sampler = SamplerJointIndep$new(list(
    Sampler1DCateg$new(search_space$params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  tuner = tnr("hyperband", eta = 2L, sampler = sampler)
  expect_error(tuner$optimize(instance), 
    regexp = "Assertion on 'sampler$param_set$ids()' failed: Must be equal to set {'eta','booster'}, but is {'booster'}.",
    fixed = TRUE)

  # budget param defined
  sampler = SamplerJointIndep$new(list(
    Sampler1D$new(search_space$params[[1]]),
    Sampler1DRfun$new(search_space$params[[2]], function(n) rbeta(n, 2, 5)),
    Sampler1DCateg$new(search_space$params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  tuner = tnr("hyperband", eta = 2L, sampler = sampler)
  expect_error(tuner$optimize(instance), 
    regexp = "Assertion on 'sampler$param_set$ids()' failed: Must be equal to set {'eta','booster'}, but is {'nrounds','eta','booster'}.",
    fixed = TRUE)
})


test_that("TunerHyperband throws an error if budget parameter is invalid", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  # non-numeric budget parameter
  search_space = ps(
    nrounds = p_int(lower = 1, upper = 8),
    eta = p_dbl(lower = 0, upper = 1),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart"), tags = "budget")
  )

  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = lrn("classif.xgboost"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("none"),
    search_space = search_space)

  tuner = tnr("hyperband", eta = 2L)
  expect_error(tuner$optimize(instance), 
    regexp = "Assertion on 'ps$class[[budget_id]]' failed: Must be element of set {'ParamInt','ParamDbl'}, but is 'ParamFct'", 
    fixed = TRUE)

  # two budget parameters
  search_space = ps(
    nrounds = p_int(lower = 1, upper = 8, tags = "budget"),
    eta = p_dbl(lower = 0, upper = 1, tags = "budget"),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
  )

  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = lrn("classif.xgboost"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), terminator = trm("none"),
    search_space = search_space)

  tuner = tnr("hyperband", eta = 2L)
  expect_error(tuner$optimize(instance), 
    regexp = "Exactly one hyperparameter must be tagged with 'budget'", fixed = TRUE)
})
