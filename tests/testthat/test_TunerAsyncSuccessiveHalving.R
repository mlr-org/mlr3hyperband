skip_on_cran()
skip_if_not_installed("rush")

test_that("TunerAsyncSuccessiveHalving works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner)

  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with eta = 3", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 2187, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 3, learner)

  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with minimum budget > 1", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(2, 8, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner)
  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with minimum budget > 1 and eta = 3", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(9, 2187, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 3, learner)

  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving rounds budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 7, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner)
  expect_integerish(instance$archive$data$iter)
  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with eta = 2.5", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 8, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2.5, learner)
  expect_integerish(instance$archive$data$iter)

  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with xgboost", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  learner = lrn("classif.xgboost",
    nrounds   = to_tune(p_int(1, 16, tags = "budget")),
    eta       = to_tune(1e-4, 1, logscale = TRUE),
    max_depth = to_tune(1, 2))

  instance = test_tuner_async_successive_halving(eta = 2, learner)
  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.debug"))
  graph_learner$param_set$values$classif.debug.x = to_tune()
  graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(lower = 1/9, upper = 1, tags = "budget"))

  instance = test_tuner_async_successive_halving(eta = 3, graph_learner)
  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with multi-crit", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with custom sampler", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$subset("x"), function(n) rbeta(n, 2, 5))

  instance = test_tuner_async_successive_halving(eta = 2, learner, sampler = sampler)
  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving errors if not enough parameters are sampled", {
  flush_redis()
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  learner = lrn("classif.debug",
    x  = to_tune(),
    message_train = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$subset("x"), function(n) rbeta(n, 2, 5))

  expect_error(tune(
    tnr("async_successive_halving", sampler = sampler),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "set",
    fixed = TRUE
  )

  mirai::daemons(0)
})

test_that("TunerAsyncSuccessiveHalving errors if budget parameter is sampled", {
  flush_redis()
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]] %??% learner$param_set$search_space()$subset("x"), function(n) rbeta(n, 2, 5)),
    Sampler1D$new(learner$param_set$search_space()$params[["iter"]] %??% learner$param_set$search_space()$subset("iter"))
  ))

  expect_error(tune(
    tnr( "async_successive_halving", sampler = sampler),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "set",
    fixed = TRUE
  )

  mirai::daemons(0)
})

test_that("TunerAsyncSuccessiveHalving errors if budget parameter is not numeric", {
  flush_redis()
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  learner = lrn("classif.debug",
    x  = to_tune(),
    predict_missing_type = to_tune(p_fct(levels = c("na", "omit"), tags = "budget"))
  )

  expect_error(tune(
    tnr("async_successive_halving"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "set",
    fixed = TRUE
  )

  mirai::daemons(0)
})

test_that("TunerAsyncSuccessiveHalving errors if multiple budget parameters are set", {
  flush_redis()
  mirai::daemons(2)
  rush::rush_plan(n_workers = 2, worker_type = "remote")

  learner = lrn("classif.debug",
    x  = to_tune(p_dbl(0, 1, tags = "budget")),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  expect_error(tune(
    tnr("async_successive_halving"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "tagged ",
    fixed = TRUE
  )

  mirai::daemons(0)
})

test_that("TunerAsyncSuccessiveHalving minimizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = TRUE), n_workers = 1)

  Sys.sleep(1)

  perf_1 = instance$archive$data[1, dummy]
  perf_2 = instance$archive$data[6, dummy]

  # if the performance of second configuration in the first stage is better than the first configuration it must be promoted to the next stage
  if (perf_2 < perf_1) {
    expect_equal(instance$archive$data[7, stage], 2)
  } else {
    expect_equal(instance$archive$data[7, stage], 1)
  }

  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving maximizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = FALSE), n_workers = 1)

  Sys.sleep(1)

  perf_1 = instance$archive$data[1, dummy]
  perf_2 = instance$archive$data[6, dummy]

  # if the performance of second configuration in the first stage is better than the first configuration it must be promoted to the next stage
  if (perf_2 > perf_1) {
    expect_equal(instance$archive$data[7, stage], 2)
  } else {
    expect_equal(instance$archive$data[7, stage], 1)
  }

  expect_rush_reset(instance$rush)
})

test_that("TunerAsyncSuccessiveHalving works with single budget value", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 1, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner)
  expect_rush_reset(instance$rush)
})

test_that("TunerAsynSuccessiveHalving works with multi-crit", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  instance = test_tuner_async_successive_halving(eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
  expect_rush_reset(instance$rush)
})
