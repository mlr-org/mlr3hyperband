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
test_tuner_hyperband = function(eta, n_dim = 1L, term_evals = NULL, lower_budget, upper_budget, measures = "classif.ce", 
  learner = lrn("classif.xgboost"), task = tsk("pima"), search_space = NULL) {

  terminator = if (is.null(term_evals)) {
    trm("none")
  } else {
    trm("evals", n_evals = term_evals)
  }

  if (is.null(search_space)) {
    if (n_dim == 1) {
      search_space = ps(
        nrounds = p_int(lower = lower_budget, upper = upper_budget, tags = "budget"),
        max_depth = p_int(lower = 1, upper = 100)
      )
    } else if (n_dim == 2) {
      search_space = ps(
        nrounds = p_int(lower = lower_budget, upper = upper_budget, tags = "budget"),
        eta = p_dbl(lower = 0, upper = 1),
        max_depth = p_int(lower = 1, upper = 100)
      )
    }
  }

  if (length(measures) == 1) {
    instance = TuningInstanceSingleCrit$new(task = task, learner = learner, resampling =  rsmp("holdout"), 
      measure = msr(measures), terminator = terminator, search_space = search_space, store_models = TRUE)
  } else {
    instance = TuningInstanceMultiCrit$new(task = task, learner = learner, resampling = rsmp("holdout"), 
      measures = lapply(measures, msr), terminator = terminator, search_space = search_space, store_models = TRUE)
  }

  tuner = tnr("hyperband", eta = eta)
  expect_tuner(tuner)

  expect_data_table(tuner$optimize(instance))
  archive = instance$archive

  # check evaluated against theoretical brackets and stages
  if (!instance$is_terminated) {
    expect_hyperband_brackets(eta, lower_budget, upper_budget, archive)
  } else {
    expect_data_table(archive$data, min.rows = term_evals)
    expect_gte(archive$n_evals, term_evals)
  }

  result_x_domain = instance$result_x_domain
  result_y = instance$result_y

  expect_list(result_x_domain)
  expect_named(instance$result_x_search_space, search_space$ids())
  
  if (inherits(instance, "TuningInstanceMultiCrit")) {
    expect_list(result_x_domain[[1]])
    expect_subset(names(result_x_domain[[1]]), instance$objective$domain$ids())
    expect_data_table(result_y, ncols = length(measures))
  } else {
    expect_subset(names(result_x_domain), instance$objective$domain$ids())
    expect_number(result_y)
  }
  list(tuner = tuner, instance = instance)
}

# calcuate hyperband brackets and stages based on paper
hyperband_brackets = function(R, eta) {
  result = data.frame()
  smax = floor(log(R, eta))
  B = (smax + 1) * R

  for (s in smax:0) {
    n = ceiling((B / R) * ((eta^s) / (s + 1)))
    r = R * eta^(-s)

    for (i in 0:s) {
      ni = floor(n * eta^(-i))
      ri = r * eta^i
      result = rbind(result, c(s, i, ri, ni))
    }
  }
  set_names(result, c("bracket", "bracket_stage", "budget_scaled", "n_configs"))
}

expect_hyperband_brackets = function(eta, lower_budget, upper_budget, archive) {
  hb_meta_info = hyperband_brackets(R = upper_budget / lower_budget, eta = eta)
  hb_meta_info = as.data.table(hb_meta_info)
  tuner_info = archive$data[, colnames(hb_meta_info), with = FALSE]
  tuner_info = unique(tuner_info) # info for all x is duplicated in each bracket
  real_evals = sum(tuner_info$n_configs)

  expect_equal(hb_meta_info, tuner_info)
  expect_equal(real_evals, archive$n_evals)
  expect_data_table(archive$data, nrows = real_evals)
}

test_tuner_successive_halving = function(n, eta, sampler = NULL, n_dim = 1L,
  lower_bound = 1, upper_bound = 16, task = tsk("pima"),
  learner = lrn("classif.xgboost"), resampling = rsmp("holdout"),
  measures = msr("classif.ce"), search_space = NULL, terminator = trm("none"),
  store_models = TRUE) {

  if(is.null(search_space)) {
    if(n_dim == 1) {
      search_space = ParamSet$new(params = list(
        ParamInt$new("nrounds", lower = lower_bound, upper = upper_bound, tags = "budget"),
        ParamInt$new("max_depth", lower = 1, upper = 100)
      ))
    } else if (n_dim == 2) {
      search_space = ParamSet$new(params = list(
        ParamInt$new("nrounds", lower = lower_bound, upper = upper_bound, tags = "budget"),
        ParamDbl$new("eta", lower = 0, upper = 1),
        ParamInt$new("max_depth", lower = 1, upper = 100)
      ))
    }
  }

  if (length(measures) == 1) {
    instance = TuningInstanceSingleCrit$new(task, learner, resampling, measures, terminator, search_space, 
      store_models = store_models, check_values = TRUE)
  } else {
    instance = TuningInstanceMultiCrit$new(task, learner, resampling, measures, terminator, search_space, 
      store_models = store_models, check_values = TRUE)
  }

  tuner = tnr("successive_halving", n = n, eta = eta, sampler = sampler)
  expect_tuner(tuner)

  tuner$optimize(instance)
  archive = instance$archive$data()

  budget = archive[, search_space$ids(tags = "budget"), with = FALSE]
  expect_lte(max(budget), upper_bound)
  expect_gte(min(budget), lower_bound)
}



# test hyperband tuner with depedencies in parameters
test_tuner_hyperband_dependencies = function(eta, term_evals = NULL, lower_budget, upper_budget) {
  ll = LearnerRegrDepParams$new()
  ll$param_set$add(ParamInt$new("nrounds", lower = lower_budget, upper = upper_budget, tags = "budget"))
  test_tuner_hyperband(eta = eta, term_evals = term_evals, lower_budget = lower_budget, upper_budget = upper_budget, 
    measures = "regr.mse", learner = ll, task = tsk("boston_housing"), search_space = ll$param_set)
}
