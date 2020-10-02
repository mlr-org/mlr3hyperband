lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

# calculating bracket meta info for given R and eta based on hb paper
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

expect_tuner = function(tuner) {
  expect_r6(tuner, "Tuner",
    public = c("optimize", "param_set"),
    private = ".optimize"
  )
  expect_is(tuner$param_set, "ParamSet")
  expect_function(tuner$optimize, args = "inst")
}

expect_info = function(eta, lower_b, upper_b, archive) {
  hb_meta_info = hyperband_brackets(R = upper_b / lower_b, eta = eta)
  hb_meta_info = as.data.table(hb_meta_info)
  tuner_info = archive$data()[, colnames(hb_meta_info), with = FALSE]
  tuner_info = unique(tuner_info) # info for all x is duplicated in each bracket
  real_evals = sum(tuner_info$n_configs)

  expect_equal(hb_meta_info, tuner_info)
  expect_equal(real_evals, archive$n_evals)
  expect_data_table(archive$data(), nrows = real_evals)
}

# single crit test
test_tuner_hyperband = function(eta, n_dim = 1L, term_evals = NULL, lower_b,
  upper_b, measures = "classif.ce", learner = lrn("classif.xgboost"),
  task = tsk("pima"), ps = NULL) {

  # run for an (almost) arbitrary time if NULL is given

  if (is.null(term_evals)) {
    term = trm("none")
  } else {
    term = trm("evals", n_evals = term_evals)
  }

  if (is.null(ps)) {
    if (n_dim == 1) {
      ps = ParamSet$new(params = list(
        ParamInt$new("nrounds", lower = lower_b, upper = upper_b,
          tags = "budget"),
        ParamInt$new("max_depth", lower = 1, upper = 100)
      ))
    } else if (n_dim == 2) {
      ps = ParamSet$new(params = list(
        ParamInt$new("nrounds", lower = lower_b, upper = upper_b,
          tags = "budget"),
        ParamDbl$new("eta", lower = 0, upper = 1),
        ParamInt$new("max_depth", lower = 1, upper = 100)
      ))
    }
  }

  if (length(measures) == 1) {
    inst = TuningInstanceSingleCrit$new(
      task, learner, rsmp("holdout"),
      msr(measures), ps, term, store_models = TRUE)
  } else {
    inst = TuningInstanceMultiCrit$new(
      task, learner, rsmp("holdout"),
      lapply(measures, msr), ps, term, store_models = TRUE)
  }

  tuner = tnr("hyperband", eta = eta)
  expect_tuner(tuner)

  tuner$optimize(inst)
  archive = inst$archive

  # compare results with full hyperband brackets if tuner was fully evaluated
  if (!inst$is_terminated) {
    expect_info(eta, lower_b, upper_b, archive)
  } else {
    expect_data_table(archive$data(), min.rows = term_evals)
    expect_gte(archive$n_evals, term_evals)
  }

  sc = inst$result_x_domain
  sp = inst$result_y

  expect_list(sc)
  expect_names(colnames(inst$result_x_search_space), identical.to = ps$ids())
  if (inherits(inst, "TuningInstanceMultiCrit")) {
    expect_list(sc[[1]])
    expect_names(names(sc[[1]]), subset.of = inst$objective$domain$ids())
    expect_data_table(sp, ncols = length(measures))
  } else {
    expect_names(names(sc), subset.of = inst$objective$domain$ids())
    expect_number(sp)
  }
  list(tuner = tuner, inst = inst)
}

# test an implemented subclass tuner by running a test with dependent params
# returns: tune_result and instance
test_tuner_hyperband_dependencies = function(eta, term_evals = NULL, lower_b,
  upper_b) {
  ll = LearnerRegrDepParams$new()
  ll$param_set$add(
    ParamInt$new("nrounds", lower = lower_b, upper = upper_b, tags = "budget")
  )
  test_res = test_tuner_hyperband(eta = eta, term_evals = term_evals,
    lower_b = lower_b, upper_b = upper_b, measures = "regr.mse", learner = ll,
    task = tsk("boston_housing"), ps = ll$param_set)
  test_res
}

# create inst object with dependencies
PS_DEPS = function() {
  ps = ParamSet$new(
    params = list(
      ParamFct$new("xx", levels = c("a", "b"), default = "a"),
      ParamDbl$new("yy", lower = 0, upper = 1),
      ParamDbl$new("cp", lower = 0, upper = 1)
    )
  )
  ps$add_dep("yy", on = "xx", cond = CondEqual$new("a"))
  return(ps)
}

# a dummy measure which simply returns the cp value of rpart
# this allows us to 'fake' performance values in unit tests during tuning
make_dummy_cp_measure = function(type) {
  if (type == "classif") {
    id = "dummy.cp.classif"
    inh = MeasureClassif
    cl = "MeaureDummyCPClassif"
  } else {
    id = "dummy.cp.regr"
    inh = MeasureRegr
    cl = "MeaureDummyCPRegr"
  }
  m = R6Class(cl,
    inherit = inh,
    public = list(
      # allow a fun to transform cp to score, this allows further shenenigans
      # to disentangle cp value and score
      fun = NULL,

      initialize = function(fun = identity) {
        super$initialize(
          id = id,
          range = c(0, Inf),
          minimize = TRUE,
          packages = "Metrics",
          properties = "requires_learner"
        )
        self$fun = fun # allow a fun to transform cp to score
      }
    ),

    private = list(
      .score = function(prediction, learner, ...) {
        self$fun(learner$param_set$values$cp)
      }
    )
  )
}
MeasureDummyCPClassif = make_dummy_cp_measure("classif")
mlr3::mlr_measures$add("dummy.cp.classif", MeasureDummyCPClassif)
MeasureDummyCPRegr = make_dummy_cp_measure("regr")
mlr3::mlr_measures$add("dummy.cp.regr", MeasureDummyCPRegr)

LearnerRegrDepParams = R6Class("LearnerRegrDepParams",
  inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.depparams") {
      param_set = PS_DEPS()
      super$initialize(
        id = id,
        feature_types = c(
          "logical", "integer", "numeric", "character",
          "factor", "ordered"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("missings")
      )
    }
  ),

  private = list(
    .train = function(task) {
      tn = task$target_names
      return(list())
    },

    .predict = function(task) {
      n = task$nrow
      response = rep(99, n)
      PredictionRegr$new(task, response = response)
    }
  )

)



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
    instance = TuningInstanceSingleCrit$new(task, learner, resampling,
      measures, search_space, terminator, store_models = store_models,
      check_values = TRUE)
  } else {
    instance = TuningInstanceMultiCrit$new(task, learner, resampling,
      measures, search_space, terminator, store_models = store_models,
      check_values = TRUE)
  }

  tuner = tnr("successive_halving", n = n, eta = eta, sampler = sampler)
  expect_tuner(tuner)

  tuner$optimize(instance)
  archive = instance$archive$data()

  budget = archive[, search_space$ids(tags = "budget"), with = FALSE]
  expect_lte(max(budget), upper_bound)
  expect_gte(min(budget), lower_bound)
}



