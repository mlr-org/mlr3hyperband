test_that("TunerAsha works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerAsha works with r_max > n", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 17, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("TunerAsha works with r_max < n", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 15, tags = "budget"))
  )

  test_tuner_successive_halving(n = 16, eta = 2, learner)
})

test_that("repeating successive halving works", {
  learner = lrn("classif.debug",
    x  = to_tune(),
    iter = to_tune(p_int(1, 16, tags = "budget"))
  )

  instance = tune(
    method = "successive_halving",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    repeats = TRUE,
    term_evals = 62)

  expect_equal(nrow(instance$archive$data), 62)

  instance = tune(
    method = "successive_halving",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    repeats = FALSE)

  expect_equal(nrow(instance$archive$data), 31)
})
