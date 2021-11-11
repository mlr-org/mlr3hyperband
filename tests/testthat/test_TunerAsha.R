test_that("TunerAsha works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  test_tuner_asha(eta = 2, learner)
})

test_that("TunerAsha works with minimum budget greater than 1", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(2, 16, tags = "budget"))
  )

  test_tuner_asha(eta = 2, learner)
})

test_that("TunerAsha works with minimum budget greater than 1", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(2, 16, tags = "budget"))
  )

  test_tuner_asha(eta = 2, learner)
})

test_that("TunerAsha rounds budget", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 15, tags = "budget"))
  )

  test_tuner_asha(eta = 2, learner)
})

test_that("TunerAsha works with eta = 2.5", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  test_tuner_asha(eta = 2.5, learner)
})

test_that("TunerAsha works with subsampling", {
  skip_if_not_installed("mlr3pipelines")
  library(mlr3pipelines)

  graph_learner = as_learner(po("subsample") %>>% lrn("classif.rpart"))
  graph_learner$param_set$values$classif.rpart.cp = to_tune(1e-04, 1e-1, logscale = TRUE)
  graph_learner$param_set$values$classif.rpart.minsplit = to_tune(2, 128, logscale = TRUE)
  graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(lower = 1/9, upper = 1, tags = "budget"))

  test_tuner_asha(eta = 3, graph_learner)
})

