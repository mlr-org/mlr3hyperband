test_that("TunerHyperband works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner)
})

test_that("TunerHyperband works with minimum budget > 1", {
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

test_that("TunerHyperband works works with multi-crit", {
  skip_if_not_installed("emoa")
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
})

test_that("TunerHyperband works with custom sampler", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]], function(n) rbeta(n, 2, 5))

  test_tuner_hyperband(eta = 2, learner, sampler = sampler)
})

test_that("TunerHyperband errors if not enough parameters are sampled", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    message_train = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]], function(n) rbeta(n, 2, 5))

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    sampler = sampler),
    regexp = "Must be equal to set",
    fixed = TRUE
  )
})

test_that("TunerHyperband errors if budget parameter is sampled", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]], function(n) rbeta(n, 2, 5)),
    Sampler1D$new(learner$param_set$search_space()$params[["iter"]])
  ))

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    sampler = sampler),
    regexp = "Must be equal to set",
    fixed = TRUE
  )
})

test_that("TunerHyperband errors if budget parameter is not numeric", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    predict_missing_type = to_tune(p_fct(levels = c("na", "omit"), tags = "budget"))
  )

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "Must be element of set",
    fixed = TRUE
  )
})

test_that("TunerHyperband errors if multiple budget parameters are set", {
  learner = lrn("classif.debug",
    x  = to_tune(p_dbl(0, 1, tags = "budget")),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  expect_error(tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "Exactly one parameter must be tagged ",
    fixed = TRUE
  )
})

test_that("TunerHyperband minimizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_hyperband(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = TRUE))
  expect_equal(min(instance$archive$data[bracket == 4 & stage == 0, dummy]),
    instance$archive$data[bracket == 4 & stage == 4, dummy])
})

test_that("TunerHyperband maximizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_hyperband(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = FALSE))
  expect_equal(max(instance$archive$data[bracket == 4 & stage == 0, dummy]),
    instance$archive$data[bracket == 4 & stage == 4, dummy])
})

test_that("TunerHyperband works with single budget value", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 1, tags = "budget"))
  )

  test_tuner_hyperband(eta = 2, learner)
})

test_that("TunerHyperband works with repetitions", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    repetitions = 2)

  expect_equal(nrow(instance$archive$data), 144)
})

test_that("TunerHyperband terminates itself", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"))

  expect_equal(nrow(instance$archive$data), 72)
})

test_that("TunerHyperband works with infinite repetitions", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 160,
    repetitions = Inf)

  expect_equal(nrow(instance$archive$data), 160)
})
