test_that("TunerHyperband works", {
  # minsplit is misused as a budget parameter
  # xgboost has a real budget parameter but evaluation takes longer

  # default
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit  = p_int(1, 16, tags = "budget"),
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_hyperband(eta = 2, learner, search_space)

  # minimum budget different from 1
  search_space = ps(
    minsplit  = p_int(2, 16, tags = "budget"),
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_hyperband(eta = 2, learner, search_space)

  # eta = 3
  search_space = ps(
    minsplit  = p_int(1, 81, tags = "budget"),
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_hyperband(eta = 3, learner, search_space)

  # budget is rounded
  search_space = ps(
    minsplit  = p_int(1, 15, tags = "budget"),
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_hyperband(eta = 2, learner, search_space)

  # eta = 2.5
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit  = p_int(1, 16, tags = "budget"),
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_hyperband(eta = 2.5, learner, search_space)

  # multi-crit
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit  = p_int(1, 16, tags = "budget"),
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_hyperband(eta = 2, learner, search_space, msrs(c("classif.ce", "classif.acc")))
})

test_that("TunerHyperband works with xgboost", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  learner = lrn("classif.xgboost")
  search_space = ps(
    nrounds   = p_int(1, 16, tags = "budget"),
    eta       = p_dbl(1e-4, 1, logscale = TRUE),
    max_depth = p_int(1, 2))

  test_tuner_hyperband(eta = 2, learner, search_space)
})

test_that("TunerHyperband works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.rpart"))
  search_space = ps(
    classif.rpart.cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    classif.rpart.minsplit  = p_int(2, 128, logscale = TRUE),
    subsample.frac          = p_dbl(lower = 0.1, upper = 1, tags = "budget"))

  test_tuner_hyperband(eta = 2, graph_learner, search_space, msr("classif.ce"))
})

test_that("TunerHyperband works with custom sampler", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  # default
  learner = lrn("classif.xgboost")
  search_space = ps(
    nrounds = p_int(lower = 1, upper = 8, tags = "budget"),
    eta     = p_dbl(lower = 0, upper = 1),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart")))

  # create custom sampler
  # beta distribution with alpha = 2 and beta = 5
  # categorical distribution with custom probabilities
  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(search_space$params[[2]], function(n) rbeta(n, 2, 5)),
    Sampler1DCateg$new(search_space$params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    search_space = search_space,
    sampler = sampler
  )

  # not enough parameters defined
  sampler = SamplerJointIndep$new(list(
    Sampler1DCateg$new(search_space$params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    search_space = search_space,
    sampler = sampler),
    regexp = "Assertion on 'sampler$param_set$ids()' failed:"
  )

  # budget parameter defined
  sampler = SamplerJointIndep$new(list(
    Sampler1D$new(search_space$params[[1]]),
    Sampler1DRfun$new(search_space$params[[2]], function(n) rbeta(n, 2, 5)),
    Sampler1DCateg$new(search_space$params[[3]], prob = c(0.2, 0.3, 0.5))
  ))

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    search_space = search_space,
    sampler = sampler),
    regexp = "Assertion on 'sampler$param_set$ids()' failed",
  )
})

test_that("TunerHyperband throws an error if budget parameter is invalid", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  # non-numeric budget parameter
  learner = lrn("classif.xgboost")
  search_space = ps(
    nrounds = p_int(lower = 1, upper = 8),
    eta     = p_dbl(lower = 0, upper = 1),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart"), tags = "budget")
  )

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    search_space = search_space),
    regexp = "but is 'ParamFct'.",
    fixed = TRUE)

  # two budget parameters
  search_space = ps(
    nrounds = p_int(lower = 1, upper = 8, tags = "budget"),
    eta     = p_dbl(lower = 0, upper = 1, tags = "budget"),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
  )

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    search_space = search_space),
    regexp = "Exactly one parameter must be tagged with 'budget'",
    fixed = TRUE)
})

test_that("repeating hyperband works", {
  learner = lrn("classif.rpart",
    minsplit  = to_tune(p_int(1, 16, tags = "budget")),
    cp        = to_tune(1e-04, 1e-1, logscale = TRUE),
    minbucket = to_tune(1, 64, logscale = TRUE))

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    repeats = TRUE,
    term_evals = 144)

  expect_equal(nrow(instance$archive$data), 144)

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    repeats = FALSE)

  expect_equal(nrow(instance$archive$data), 72)
})
