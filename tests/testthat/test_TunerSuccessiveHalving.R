test_that("TunerSuccessiveHalving works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving works with minimum budget > 1", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(2, 8, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving rounds budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 7, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving works with eta = 2.5", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 8, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2.5, learner)
})

test_that("TunerSuccessiveHalving adjusts minimum budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 30, tags = "budget"))
  )

  instance = test_tuner_successive_halving(n = 30, eta = 3, learner, adjust_minimum_budget = TRUE)
  expect_equal(max(instance$archive$data$iter), 30)

  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 30, tags = "budget"))
  )

  instance = test_tuner_successive_halving(n = 30, eta = 3, learner, adjust_minimum_budget = FALSE)
  expect_equal(max(instance$archive$data$iter), 27)
})

test_that("TunerSuccessiveHalving works with xgboost", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  learner = lrn("classif.xgboost",
    nrounds   = to_tune(p_int(1, 16, tags = "budget")),
    eta       = to_tune(1e-4, 1, logscale = TRUE),
    max_depth = to_tune(1, 2))

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.debug"))
  graph_learner$param_set$values$classif.debug.x = to_tune()
  graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(lower = 1/9, upper = 1, tags = "budget"))

  test_tuner_successive_halving(n = 81, eta = 3, graph_learner)
})

test_that("TunerSuccessiveHalving works with multi-crit", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
})

test_that("TunerSuccessiveHalving works with custom sampler", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]] %??% learner$param_set$search_space()$subset("x"), function(n) rbeta(n, 2, 5))

  test_tuner_successive_halving(n = 16, eta = 2, learner, sampler = sampler)
})

test_that("TunerSuccessiveHalving errors if not enough parameters are sampled", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    message_train = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]] %??% learner$param_set$search_space()$subset("x"), function(n) rbeta(n, 2, 5))

  expect_error(tune(
    tnr( "successive_halving", sampler = sampler),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "set",
    fixed = TRUE
  )
})

test_that("TunerSuccessiveHalving errors if budget parameter is sampled", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]] %??% learner$param_set$search_space()$subset("x"), function(n) rbeta(n, 2, 5)),
    Sampler1D$new(learner$param_set$search_space()$params[["iter"]] %??% learner$param_set$search_space()$subset("iter"))
  ))

  expect_error(tune(
    tnr( "successive_halving", sampler = sampler),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "set",
    fixed = TRUE
  )
})

test_that("TunerSuccessiveHalving errors if budget parameter is not numeric", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    predict_missing_type = to_tune(p_fct(levels = c("na", "omit"), tags = "budget"))
  )

  expect_error(tune(
    tnr( "successive_halving"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "set",
    fixed = TRUE
  )
})

test_that("TunerSuccessiveHalving errors if multiple budget parameters are set", {
  learner = lrn("classif.debug",
    x  = to_tune(p_dbl(0, 1, tags = "budget")),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  expect_error(tune(
    tnr( "successive_halving"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "tagged ",
    fixed = TRUE
  )
})

test_that("TunerSuccessiveHalving minimizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_successive_halving(n = 16, eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = TRUE))
  expect_equal(min(instance$archive$data[stage == 0, dummy]), instance$archive$data[stage == 4, dummy])
})

test_that("TunerSuccessiveHalving maximizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_successive_halving(n = 16, eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = FALSE))
  expect_equal(max(instance$archive$data[stage == 0, dummy]), instance$archive$data[stage == 4, dummy])
})

test_that("TunerSuccessiveHalving works with single budget value", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 1, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving works with repetitions", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    tnr( "successive_halving", repetitions = 2),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"))

  expect_equal(nrow(instance$archive$data), 62)
})

test_that("TunerSuccessiveHalving terminates itself", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    tnr( "successive_halving"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"))

  expect_equal(nrow(instance$archive$data), 31)
})

test_that("TunerSuccessiveHalving works with infinite repetitions", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    tnr( "successive_halving", repetitions = Inf),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 70)

  expect_equal(nrow(instance$archive$data), 78)
})

test_that("TunerSuccessiveHalving works with r_max > n", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 17, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving works with r_max < n", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 15, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerSuccessiveHalving works with r_max < n and adjust minimum budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 15, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner, adjust_minimum_budget = TRUE)
})

test_that("TunerSuccessiveHalving man exists", {
  expect_man_exists(tnr("successive_halving")$man)
})
