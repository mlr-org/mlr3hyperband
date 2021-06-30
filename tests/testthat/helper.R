# nolint start
library(checkmate)
library(mlr3)
library(mlr3misc)
library(mlr3pipelines)
library(paradox)
library(R6)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3tuning"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)
# nolint end

#' @title Hyperband Brackets
#' 
#' @noRd 
#' 
#' @description 
#' Calculates hyperband brackets and stages for a given `R` and `eta`. 
#' 
#' @param R (`numeric(1)`)\cr
#' Maximum budget a single configuration in the last stage.
#' @param eta (`numeric(1)`).
#' @param r_scale (`numeric(1)`)\cr
#' Scaling factor to retransform budget to original scale.
#' @param round (`logical(1)`)\cr
#' Determines if budget is an integer.
hyperband_brackets = function(R, eta, r_scale = 1, round) {
  s_max = floor(log(R, eta))
  B = (s_max + 1) * R

  map_dtr(s_max:0, function(s) {
    n = ceiling((B / R) * ((eta^s) / (s + 1)))
    r = R * eta^(-s)
    map_dtr(0:s, function(i) {
      ni = floor(n * eta^(-i))
      ri = r_scale * r * eta^i
      if (round) ri = round(ri)
      data.table(bracket = s, stage = i, budget = ri, n = ni)
    })
  })
}

#' @title Test Tuner Hyperband
#' 
#' @noRd 
#' 
#' @description 
#'  
#' @param eta (`numeric(1)`).
test_tuner_hyperband = function(eta, learner, search_space, measures = msr("classif.ce")) {
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]

  instance = tune(
    method = "hyperband",
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("holdout"),
    search_space = search_space,
    eta = eta
  )

  # compare brackets and stages of tuner to theoretical hyperband 
  plan_tuner = as.data.table(instance$archive)[, .N, by = c("bracket", "stage", budget_id)]
  round = search_space$class[[budget_id]] == "ParamInt"
  plan_hyperband = hyperband_brackets(r_max / r_min, eta, r_min, round)

  expect_equal(plan_tuner$bracket, plan_hyperband$bracket)
  expect_equal(plan_tuner$stage, plan_hyperband$stage)
  expect_equal(plan_tuner[[budget_id]], plan_hyperband$budget)
  expect_equal(plan_tuner$N, plan_hyperband$n)

  instance
}

#' @title Test Tuner Successive Halving
#' 
#' @noRd 
#' 
#' @description 
#'  
#' @param eta (`numeric(1)`).
test_tuner_successive_halving = function(n, eta, learner, search_space, measures = msr("classif.ce")) {
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]

  instance = tune(
    method = "successive_halving",
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("holdout"),
    search_space = search_space,
    n = n,
    eta = eta)

    budget = as.data.table(instance$archive)[, budget_id, with = FALSE]
    n_configs = as.data.table(instance$archive)[, .N, by = "stage"]

    # check bounds of budget
    expect_lte(max(budget), r_max)
    expect_gte(min(budget), r_min)
    # check number of configs
    expect_lte(max(n_configs$N), n)
}
