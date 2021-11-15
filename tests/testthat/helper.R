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

#' @title Test Tuner Hyperband
#'
#' @noRd
#'
#' @description
#' Tests bracket and stages constructed by the tuner against the ones based on
#' the original hyperband paper.
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
  plan_hyperband = hyperband_schedule(r_min, r_max, eta, round)

  expect_set_equal(plan_tuner$bracket, plan_hyperband$bracket)
  expect_set_equal(plan_tuner$stage, plan_hyperband$stage)
  expect_set_equal(plan_tuner[[budget_id]], plan_hyperband$budget)
  expect_set_equal(plan_tuner$N, plan_hyperband$n)

  instance
}

#' @title Test Tuner Successive Halving
#'
#' @noRd
#'
#' @description
#' Tests budget and number of configs constructed by the tuner against supplied
#' bounds
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

    instance
}

#' @title Test Tuner Asha
#'
#' @noRd
#'
#' @description
#' Tests budget and number of configs constructed by the tuner against supplied
#' bounds
test_tuner_asha = function(eta, learner, measures = msr("classif.ce"), term_evals = 15, allow_hotstart = FALSE,
  keep_hotstart_stack = TRUE, early_stopping_rate = 0) {

  search_space = learner$param_set$search_space()
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]

  instance = tune(
    method = "asha",
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("holdout"),
    term_evals = term_evals,
    eta = eta,
    early_stopping_rate = early_stopping_rate,
    allow_hotstart = allow_hotstart,
    keep_hotstart_stack = keep_hotstart_stack
  )

  expect_null(instance$archive$data$resample_result)
  expect_null(instance$archive$data$promise)
  expect_null(instance$archive$data$resolve_id)
  expect_true(instance$async)
  expect_r6(instance$objective, "ObjectiveTuningAsync")
  expect_integer(instance$archive$data$asha_id)
  expect_integer(instance$archive$data$stage)
  expect_gte(min(instance$archive$data[[budget_id]]), r_min)
  expect_lte(max(instance$archive$data[[budget_id]]), r_max)

  instance
}

#' @title Test Tuner Ahb
#'
#' @noRd
#'
#' @description
#' Tests budget and number of configs constructed by the tuner against supplied
#' bounds
test_tuner_ahb = function(eta, learner, measures = msr("classif.ce"), term_evals = 100, allow_hotstart = FALSE,
  keep_hotstart_stack = TRUE) {

  search_space = learner$param_set$search_space()
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]
  integer_budget = search_space$class[[budget_id]] == "ParamInt"

  instance = tune(
    method = "ahb",
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("holdout"),
    term_evals = term_evals,
    eta = eta,
    allow_hotstart = allow_hotstart,
    keep_hotstart_stack = keep_hotstart_stack
  )

  archive = as.data.table(instance$archive)
  k_max = floor(log(r_max / r_min, eta))

  expect_null(instance$archive$data$resample_result)
  expect_null(instance$archive$data$promise)
  expect_null(instance$archive$data$resolve_id)
  expect_true(instance$async)
  expect_set_equal(archive$bracket, k_max:0)
  map(k_max:0, function(k) {
    ri = r_min * eta^(k_max - k)
    if (integer_budget) ri = as.integer(round(ri))
    expect_gte(min(archive[bracket == k][[budget_id]]), ri)
    expect_lte(max(archive[bracket == k][[budget_id]]), r_max)
  })
  expect_r6(instance$objective, "ObjectiveTuningAsync")
  expect_integer(instance$archive$data$asha_id)
  expect_integer(instance$archive$data$stage)

  instance
}

#' @title MeasureClassifBudget
#'
#' @noRd
MeasureClassifDummy = R6Class("MeasureClassifDummy",
  inherit = MeasureClassif,
  public = list(
    parameter_id = NULL,

    initialize = function(parameter_id, minimize = FALSE) {
      self$parameter_id = parameter_id
      super$initialize(
        id = "budget",
        range = c(-Inf, Inf),
        minimize = minimize,
        properties = "requires_learner"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      minimize = if (self$minimize) -1 else 1
      minimize * learner$param_set$values[[self$parameter_id]]
    }
  )
)

mlr_measures$add("budget", MeasureClassifDummy)
