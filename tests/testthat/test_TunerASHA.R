test_that("hotstart works with rush" {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  config = start_flush_redis()
  future::plan("multisession", workers = 2L)
  rush = Rush$new("test", config)

  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 4, tags = "budget"))
  )

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 30),
    store_models = FALSE,
    store_benchmark_result = FALSE,
    allow_hotstart = TRUE,
    rush = rush,
    lgr_thresholds = c(rush = "debug", bbotk = "debug", mlr3 = "debug"),
    freeze_archive = FALSE
  )

  tuner = tnr("asha", eta = 3)
  tuner$optimize(instance)

  messages = rush$read_log()$msg
  messages = messages[grep("Hotstart", messages)]
  expect_true(length(messages) > 0)
})

test_that("hotstart works with rush and xgboost" {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint

  config = start_flush_redis()
  future::plan("multisession", workers = 2L)
  rush = Rush$new("test", config)

  learner = lrn("classif.xgboost",
    nrounds   = to_tune(p_int(1, 16, tags = "budget")),
    eta       = to_tune(1e-4, 1, logscale = TRUE),
    max_depth = to_tune(1, 2))

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 30),
    store_models = FALSE,
    store_benchmark_result = FALSE,
    allow_hotstart = TRUE,
    rush = rush,
    lgr_thresholds = c(rush = "debug", bbotk = "debug", mlr3 = "debug"),
    freeze_archive = FALSE
  )

  tuner = tnr("asha", eta = 3)
  expect_tuner$optimize(instance)

  messages = rush$read_log()$msg
  messages = messages[grep("Hotstart", messages)]
  expect_true(length(messages) > 0)
})
