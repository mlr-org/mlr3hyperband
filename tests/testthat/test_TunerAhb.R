test_that("TunerAhb works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 8, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner)
})

test_that("TunerAhb works with minimum budget greater than 1", {
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

test_that("TunerAsha works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.debug"))
  graph_learner$param_set$values$classif.debug.x = to_tune()
  graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(lower = 1/9, upper = 1, tags = "budget"))

  test_tuner_ahb(eta = 3, graph_learner)
})

test_that("TunerAsha works with multi-crit", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  test_tuner_ahb(eta = 2, learner, measures = msrs(c("classif.ce", "classif.acc")))
})

test_that("TunerAhb throws an error if budget parameter is invalid", {
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
    method = "ahb",
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
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    measures = msr("classif.ce"),
    resampling = rsmp("holdout"),
    search_space = search_space),
    regexp = "Exactly one parameter must be tagged with 'budget'",
    fixed = TRUE)
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
