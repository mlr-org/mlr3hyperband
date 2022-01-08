test_that("TunerAhb works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 8, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner)
})

test_that("TunerAhb works with minimum budget > 1", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(2, 8, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner)
})

test_that("TunerAhb rounds budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 7, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner)
})

test_that("TunerAhb works with eta = 2.5", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 8, tags = "budget"))
  )

  instance = test_tuner_ahb(eta = 2.5, learner)
})

test_that("TunerAsha adjusts minimum budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 10, tags = "budget"))
  )

  instance = test_tuner_ahb(eta = 3, learner, adjust_minimum_budget = TRUE)
  expect_equal(max(instance$archive$data$iter), 10)

  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 10, tags = "budget"))
  )

  instance = test_tuner_ahb(eta = 3, learner, adjust_minimum_budget = FALSE)
  expect_equal(max(instance$archive$data$iter), 9)
})

test_that("TunerAhb works with xgboost", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  learner = lrn("classif.xgboost",
    nrounds   = to_tune(p_int(1, 16, tags = "budget")),
    eta       = to_tune(1e-4, 1, logscale = TRUE),
    max_depth = to_tune(1, 2))

  test_tuner_ahb(eta = 2, learner)
})

test_that("TunerAhb works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.debug"))
  graph_learner$param_set$values$classif.debug.x = to_tune()
  graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(lower = 1/9, upper = 1, tags = "budget"))

  test_tuner_ahb(eta = 3, graph_learner)
})

test_that("TunerAhb works with multi-crit", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
})

test_that("TunerAhb works with custom sampler", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]], function(n) rbeta(n, 2, 5))

  test_tuner_ahb(eta = 2, learner, sampler = sampler)
})

test_that("TunerAhb errors if not enough parameters are sampled", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    message_train = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]], function(n) rbeta(n, 2, 5))

  expect_error(tune(
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    sampler = sampler),
    regexp = "Must be equal to set",
    fixed = TRUE
  )
})

test_that("TunerAhb errors if budget parameter is sampled", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  sampler = SamplerJointIndep$new(list(
    Sampler1DRfun$new(learner$param_set$search_space()$params[["x"]], function(n) rbeta(n, 2, 5)),
    Sampler1D$new(learner$param_set$search_space()$params[["iter"]])
  ))

  expect_error(tune(
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    sampler = sampler),
    regexp = "Must be equal to set",
    fixed = TRUE
  )
})

test_that("TunerAhb errors if budget parameter is not numeric", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    predict_missing_type = to_tune(p_fct(levels = c("na", "omit"), tags = "budget"))
  )

  expect_error(tune(
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "Must be element of set",
    fixed = TRUE
  )
})

test_that("TunerAhb errors if multiple budget parameters are set", {
  learner = lrn("classif.debug",
    x  = to_tune(p_dbl(0, 1, tags = "budget")),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  expect_error(tune(
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce")),
    regexp = "Exactly one parameter must be tagged ",
    fixed = TRUE
  )
})

test_that("TunerAhb minimizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_ahb(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = TRUE))
  expect_equal(min(instance$archive$data[c(1, 2), dummy]), instance$archive$data[3, dummy])
})

test_that("TunerAhb maximizes measure", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = test_tuner_ahb(eta = 2, learner, measures = msr("dummy", parameter_id = "x", minimize = FALSE))
  expect_equal(max(instance$archive$data[c(1, 2), dummy]), instance$archive$data[3, dummy])
})

test_that("TunerAhb works with single budget value", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 1, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner)
})

test_that("TunerAhb works with hotstarting", {
  skip_if_not_installed("RSQLite")

  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )
  instance = test_tuner_ahb(eta = 2, learner, allow_hotstart = TRUE)

  expect_file_exists(instance$objective$hotstart_stack$stack)
  expect_equal(get_private(instance$objective$hotstart_stack)$.learner_count, 100)
  expect_null(instance$archive$data$expect_resample_result)
})

test_that("TunerAhb errors if non-supported terminators are used", {
  learner = lrn("classif.debug",
    x = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  expect_error(tune(
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout")),
    regexp = "does not support",
    fixed = TRUE)
})
