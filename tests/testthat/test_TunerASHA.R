test_that("hotstart works with rush" {
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

  rush$read_log()

  con = file("log.txt")
  write.table(rush$read_log()[, list(logger, msg)], con)

})

test_that("hotstart works with rush and xgboost" {


  config = start_flush_redis()
  future::plan("multisession", workers = 2L)
  rush = Rush$new("test", config)

  learner = lts(lrn("classif.xgboost"))

  learner$param_set$set_values(nrounds = to_tune(p_int(1, 729, tags = "budget")))

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
    freeze_archive = FALSE
  )

  rush$await_workers(2)

  tuner = tnr("asha", eta = 3)
  tuner$optimize(instance)

  # FIXME: add test
  unlist(as.data.table(instance$archive)$log)
})
