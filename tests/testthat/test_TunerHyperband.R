test_that("TunerHyperband works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner)
})

test_that("TunerHyperband works with minimum budget greater than 1", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(2, 8, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner)
})

test_that("TunerHyperband rounds budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 7, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner)
})

test_that("TunerHyperband works with eta = 2.5", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 8, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2.5, learner)
})

test_that("TunerHyperband works with xgboost", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  learner = lrn("classif.xgboost",
    nrounds   = to_tune(p_int(1, 16, tags = "budget")),
    eta       = to_tune(1e-4, 1, logscale = TRUE),
    max_depth = to_tune(1, 2))
  test_tuner_hyperband(eta = 2, learner)
})

test_that("TunerHyperband works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.debug"))
  graph_learner$param_set$values$classif.debug.x = to_tune()
  graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(lower = 1/9, upper = 1, tags = "budget"))

  test_tuner_hyperband(eta = 3, graph_learner)
})

test_that("TunerHyperbandworks with multi-crit", {
  skip_if_not_installed("emoa")
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
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
    regexp = "Must be equal to set",
    fixed = TRUE)

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
    regexp = "Assertion on 'sampler$param_set$ids()' failed: Must be equal to set {'eta','booster'}, but is {'nrounds','eta','booster'}.",
    fixed = TRUE)
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

test_that("minimize works", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_hyperband(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = TRUE))
  expect_equal(min(instance$archive$data[bracket == 4 & stage == 0, dummy]),
    instance$archive$data[bracket == 4 & stage == 4, dummy])
})

test_that("maximize works", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_hyperband(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = FALSE))
  expect_equal(max(instance$archive$data[bracket == 4 & stage == 0, dummy]),
    instance$archive$data[bracket == 4 & stage == 4, dummy])
})
