test_that("TunerSuccessiveHalving works", {
  # minsplit is misused as a budget parameter
  # xgboost has a real budget parameter but evaluation takes longer

  # default
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit  = p_int(1, 16, tags = "budget"), 
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_successive_halving(n = 16, eta = 2, learner, search_space)

  # r_max > n
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit  = p_int(1, 17, tags = "budget"), 
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_successive_halving(n = 16, eta = 2, learner, search_space)

  # r_max < n
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit  = p_int(1, 15, tags = "budget"), 
    cp        = p_dbl(1e-04, 1e-1, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE))

  test_tuner_successive_halving(n = 16, eta = 2, learner, search_space)
})
