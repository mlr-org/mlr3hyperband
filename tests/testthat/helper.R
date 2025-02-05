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
test_tuner_hyperband = function(eta, learner, measures = msr("classif.ce"), sampler = NULL) {
  search_space = learner$param_set$search_space()
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]

  instance = tune(
    tnr("hyperband", eta = eta, sampler = sampler),
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("holdout"),
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
test_tuner_successive_halving = function(n, eta, learner, measures = msr("classif.ce"), sampler = NULL,
  adjust_minimum_budget = FALSE) {
  search_space = learner$param_set$search_space()
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]

  instance = tune(
    tnr("successive_halving", n = n, eta = eta, sampler = sampler, adjust_minimum_budget = adjust_minimum_budget),
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("holdout"))

    budget = as.data.table(instance$archive)[, budget_id, with = FALSE]
    n_configs = as.data.table(instance$archive)[, .N, by = "stage"]

    # check bounds of budget
    expect_lte(max(budget), r_max)
    expect_gte(min(budget), r_min)
    # check number of configs
    expect_lte(max(n_configs$N), n)

    instance
}

#' @title Test Tuner Async Successive Halving
#'
#' @noRd
#'
#' @description
#' Tests budget and number of configs constructed by the tuner against supplied bounds
test_tuner_async_successive_halving = function(eta, learner, measures = msr("classif.ce"), sampler = NULL, n_workers = 2) {
  flush_redis()
  rush::rush_plan(n_workers = n_workers)

  search_space = learner$param_set$search_space()
  budget_id = search_space$ids(tags = "budget")
  r_min = search_space$lower[[budget_id]]
  r_max = search_space$upper[[budget_id]]

  instance = tune(
    tnr("async_successive_halving", eta = eta, sampler = sampler),
    task = tsk("pima"),
    learner = learner,
    measures = measures,
    resampling = rsmp("cv", folds = 5),
    terminator = trm("evals", n_evals = 20))


  budget = as.data.table(instance$archive)[, budget_id, with = FALSE]

  # check bounds of budget
  expect_lte(max(budget), r_max)
  expect_gte(min(budget), r_min)

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
        id = "dummy",
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

mlr_measures$add("dummy", MeasureClassifDummy)

flush_redis = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
}

expect_rush_reset = function(rush, type = "kill") {
  processes = rush$processes_processx
  rush$reset(type = type)
  Sys.sleep(1)
  keys = rush$connector$command(c("KEYS", "*"))
  if (!test_list(keys, len = 0)) {
    stopf("Found keys in redis after reset: %s", keys)
  }
  walk(processes, function(p) p$kill())
}
