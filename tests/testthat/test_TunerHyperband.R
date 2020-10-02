library(mlr3learners)
library(mlr3pipelines)
library(checkmate)

context("TunerHyperband")


test_that("TunerHyperband singlecrit", {
  set.seed(1234)
  test_tuner_hyperband(eta = 3L, lower_b = 1, upper_b = 27)
  test_tuner_hyperband(eta = 2L, lower_b = 1, upper_b = 8, term_evals = 10, n_dim = 2L)
  test_tuner_hyperband_dependencies(eta = 3L, lower_b = 1, upper_b = 27)
})


test_that("TunerHyperband multicrit", {
  set.seed(1234)
  test_tuner_hyperband(eta = 3L, lower_b = 1, upper_b = 27, measures = c("classif.fpr", "classif.tpr"))
  test_tuner_hyperband(eta = 2L, lower_b = 1, upper_b = 8, term_evals = 10, n_dim = 2L, measures = c("classif.fpr", "classif.tpr"))

  params = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10, tags = "budget")
  ))

  measures = c("classif.tpr", "classif.fpr")

  inst = TuningInstanceMultiCrit$new(
    tsk("pima"),
    lrn("classif.rpart"),
    rsmp("holdout"),
    msrs(c("classif.tpr", "classif.fpr")),
    params,
    trm("evals", n_evals = 100000)
  )

  tuner = tnr("hyperband", eta = 4L)
  tuner$optimize(inst)
  # lapply(inst$pareto_front(), expect_resample_result)

  results = inst$archive$data()[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]

  expect_data_table(results, ncols = 4, nrows = 7)

  assert_data_table(inst$result)
  assert_list(inst$result_learner_param_vals, types = "list")
  assert_names(names(inst$result_learner_param_vals[[1]]), must.include = params$ids())
  assert_names(names(inst$result_x_domain[[1]]), must.include = params$ids())
  assert_names(colnames(inst$result_x_search_space), permutation.of = params$ids())
  assert_names(colnames(inst$result), must.include = c(params$ids(), measures))
})


test_that("TunerHyperband using CV", {
  set.seed(123)

  # define hyperparameter and budget parameter for tuning with hyperband
  ps = ParamSet$new(list(
    ParamInt$new("nrounds", lower = 1, upper = 8, tags = "budget"),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  ))

  # tuning instance with 2-fold CV
  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("cv", folds = 2),
    msr("classif.ce"),
    ps,
    trm("evals", n_evals = 100000)
  )

  # hyperband + tuning
  tuner = tnr("hyperband", eta = 2L)
  tuner$optimize(inst)

  results = inst$archive$data()[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]

  #FIXME: Check that results are also good, i.e. the correct individuals survive (https://github.com/mlr-org/mlr3hyperband/issues/50)
  expect_data_table(results, ncols = 3, nrows = 35)
})


test_that("TunerHyperband using subsampling", {
  set.seed(123)

  # define Graph Learner from rpart with subsampling as preprocessing step
  pops = mlr_pipeops$get("subsample")
  graph_learner = GraphLearner$new(pops %>>% lrn("classif.rpart"))
  graph_learner$properties = graph_learner$properties[graph_learner$properties %nin% "continue"]

  # define with extended hyperparameters with subsampling fraction as budget
  # ==> no learner budget is required
  params = list(
    ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
    ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
    ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
  )

  # define TuningInstanceSingleCrit with the Graph Learner and the extended hyperparams
  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    graph_learner,
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    trm("evals", n_evals = 100000)
  )

  # define and call hyperband as usual
  tuner = tnr("hyperband", eta = 4L)
  tuner$optimize(inst)

  results = inst$archive$data()[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]
  expect_data_table(results, ncols = 4, nrows = 7)
})


test_that("TunerHyperband using subsampling and non-integer eta", {
  set.seed(123)

  # define Graph Learner from rpart with subsampling as preprocessing step
  pops = po("subsample")
  graph_learner = as_learner(pops %>>% lrn("classif.rpart"))
  graph_learner$properties = graph_learner$properties[graph_learner$properties %nin% "continue"]

  # define with extended hyperparameters with subsampling fraction as budget
  # ==> no learner budget is required
  params = list(
    ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
    ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
    ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
  )

  # define TuningInstanceSingleCrit with the Graph Learner and the extended hyperparams
  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    graph_learner,
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    trm("evals", n_evals = 100000)
  )

  # define and call hyperband as usual
  tuner = tnr("hyperband", eta = 3.5)
  tuner$optimize(inst)

  results = inst$archive$data()[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]
  expect_data_table(results, ncols = 4, nrows = 7)
})


test_that("TunerHyperband using param trafo and non-integer eta", {
  set.seed(123)

  #FIXME: This Test does not what it is intendet to do

  # define hyperparameter and budget parameter for tuning with hyperband
  ps = ParamSet$new(list(
    ParamInt$new("nrounds", lower = 1, upper = 10, tags = "budget"),
    ParamDbl$new("eta", lower = 0, upper = 1),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  ))

  # ps$trafo = function(x, param_set) {
  #  x$nrounds = round(x$nrounds)
  #  return(x)
  # }

  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("holdout"),
    msr("classif.ce"),
    ps,
    trm("evals", n_evals = 100000)
  )

  # hyperband + tuning
  tuner = tnr("hyperband", eta = 3.9)
  tuner$optimize(inst)

  results = inst$archive$data()[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]
  expect_data_table(results, ncols = 4, nrows = 7)
})


test_that("TunerHyperband using custom sampler", {
  set.seed(123)

  # define hyperparameter and budget parameter for tuning with hyperband
  params = list(
    ParamInt$new("nrounds", lower = 1, upper = 8, tags = "budget"),
    ParamDbl$new("eta", lower = 0, upper = 1),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  )


  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    trm("evals", n_evals = 100000)
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
  tuner = tnr("hyperband", eta = 2L, sampler = sampler_fail1)
  expect_tuner(tuner)
  expect_error(tuner$optimize(inst), "Assertion on ")
  tuner = tnr("hyperband", eta = 2L, sampler = sampler_fail2)
  expect_tuner(tuner)
  expect_error(tuner$optimize(inst), "Assertion on ")

  # check if correct sampler works
  tuner = tnr("hyperband", eta = 2L, sampler = sampler)
  expect_tuner(tuner)
  tuner$optimize(inst)
  rr = inst$archive$benchmark_result$resample_result(uhash = inst$archive$best()$uhash)
  expect_resample_result(rr)

  results = inst$archive$data()[, .(nrounds, eta, booster, classif.ce)]
  expect_data_table(results, ncols = 4, nrows = 35)
})


test_that("TunerHyperband invalid input", {
  set.seed(123)

  # non numberish budget param
  params = list(
    ParamInt$new("nrounds", lower = 1, upper = 8),
    ParamDbl$new("eta", lower = 0, upper = 1),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"), tags = "budget")
  )


  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    trm("evals", n_evals = 100000)
  )

  tuner = tnr("hyperband", eta = 2L)
  expect_tuner(tuner)
  expect_error(tuner$optimize(inst), "Assertion on ")


  ### two budget parameters
  params = list(
    ParamInt$new("nrounds", lower = 1, upper = 8, tags = "budget"),
    ParamDbl$new("eta", lower = 0, upper = 1, tags = "budget"),
    ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
  )


  inst = TuningInstanceSingleCrit$new(
    tsk("iris"),
    lrn("classif.xgboost"),
    rsmp("holdout"),
    msr("classif.ce"),
    ParamSet$new(params),
    trm("evals", n_evals = 100000)
  )

  tuner = tnr("hyperband", eta = 2L)
  expect_tuner(tuner)
  expect_error(
    tuner$optimize(inst),
    "Exactly one hyperparameter must be tagged with 'budget'"
  )
})
