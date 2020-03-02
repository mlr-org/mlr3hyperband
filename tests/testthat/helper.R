lapply(
  list.files(
    system.file("testthat", package = "mlr3"),
    pattern = "^helper.*\\.[rR]$",
    full.names = TRUE
  ),
  source
)


# calculating bracket meta info for given R and eta based on hb paper
hyperband_brackets = function(R, eta) {

  result = data.frame()
  smax = floor(log(R, eta))
  B = (smax + 1) * R

  for (s in smax:0) {

    n = ceiling((B/R) * ((eta^s)/(s+1)))
    r = R * eta^(-s)

    for (i in 0:s) {

      ni = floor(n * eta^(-i))
      ri = r * eta^i
      result = rbind(result, c(s, i, ri, ni))
    }
  }

  names(result) = c("bracket", "bracket_stage", "budget_scaled", "n_configs")
  return(result)
}


expect_tuner = function(tuner) {
  expect_r6(tuner, "Tuner",
    public = c("tune", "param_set"),
    private = ".tune"
  )
  expect_is(tuner$param_set, "ParamSet")
  expect_function(tuner$tune, args = "instance")
}

# deprecated - remove this?
expect_resampleresult = function(resampleresult) {
  # test for existence of the most important resampling result elements
  # let's try to not be too specific since resampling is still in development
  expect_r6(resampleresult, "ResampleResult", 
    public = c("resampling", "learners", "task", "data", "predictions", "score")
  )
  expect_data_table(resampleresult$data, min.rows = 1L, min.cols = 1L)
  expect_list(resampleresult$learners, min.len = 1L)
  expect_r6(resampleresult$task)
  expect_r6(resampleresult$resampling)
}


expect_terminator = function(term) {
  expect_r6(term, "Terminator",
    public = c("is_terminated", "param_set")
  )
  expect_is(term$param_set, "ParamSet")
}

# test an implemented subclass tuner by running a couple of standard tests
# on a simple example
# term_evals: how we configure the Terminator
# real_evals: how many evals we really expect (as the optim might early stop)
# returns: tune_result and instance
test_tuner = function(key, eta, n_dim = 1L, term_evals = NULL, lower_b, upper_b, measures = "classif.ce") {

  # run for an (almost) arbitrary time if NULL is given
  if (is.null(term_evals)) term_evals = 999999

  ps = if (n_dim == 1) {

    ParamSet$new(params = list(
      ParamInt$new("nrounds",   lower = lower_b, upper = upper_b, tags = "budget"),
      ParamInt$new("max_depth", lower = 1, upper = 100)
    ))

  } else if (n_dim == 2) {

    ParamSet$new(params = list(
      ParamInt$new("nrounds",   lower = lower_b, upper = upper_b, tags = "budget"),
      ParamDbl$new("eta",       lower = 0, upper = 1),
      ParamInt$new("max_depth", lower = 1, upper = 100)
    ))
  }
  
  task = tsk("pima")

  term = term("evals", n_evals = term_evals)
  inst = TuningInstance$new(task, lrn("classif.xgboost"), rsmp("holdout"), lapply(measures, msr), ps, term)
  tuner = tnr(key, eta = eta)
  expect_tuner(tuner)

  tuner$tune(inst)
  bmr = inst$bmr

  real_evals = sum(tuner$info$n_configs)

  # compare results with full hyperband brackets if tuner was fully evaluated
  if (term_evals == 999999) {

    hb_meta_info = hyperband_brackets(R = upper_b/lower_b, eta = eta)
    hb_meta_info = as.data.table(hb_meta_info)
    tuner_info = tuner$info[, c(1:3, 5)]

    expect_equal(hb_meta_info, tuner_info)
    expect_equal(real_evals, inst$n_evals)
    expect_data_table(bmr$data, nrows = real_evals)

  } else {

    expect_data_table(bmr$data, min.rows = term_evals)
    expect_gte(inst$n_evals, term_evals)
  }


  r = inst$result
  sc = r$tune_x
  sp = r$perf

  # hotfix; remove "if" once R CMD works without
  if (TRUE) expect_list(sc, len = n_dim + 1)

  if (n_dim == 1) {
    expect_named(sc, c("nrounds", "max_depth"))
  } else if (n_dim == 2) {
    expect_named(sc, c("nrounds", "eta", "max_depth"))
  }

  expect_numeric(sp, len = length(measures))
  expect_named(sp, measures)
  list(tuner = tuner, inst = inst)
}

# test an implemented subclass tuner by running a test with dependent params
# returns: tune_result and instance
test_tuner_dependencies = function(key, eta, term_evals = NULL, lower_b, upper_b) {

  # run for an (almost) arbitrary time if NULL is given
  if (is.null(term_evals)) term_evals = 999999

  term = term("evals", n_evals = term_evals)
  ll = LearnerRegrDepParams$new()
  ll$param_set$add(
    ParamInt$new("nrounds", lower = lower_b, upper = upper_b, tags = "budget")
  )
  inst = TuningInstance$new(tsk("boston_housing"), ll, rsmp("holdout"), msr("regr.mse"), ll$param_set, term)
  tuner = tnr(key, eta)
  expect_tuner(tuner)
  tuner$tune(inst)
  bmr = inst$bmr

  real_evals = sum(tuner$info$n_configs)

  # compare results with full hyperband brackets if tuner was fully evaluated
  if (term_evals == 999999) {

    hb_meta_info = hyperband_brackets(R = upper_b/lower_b, eta = eta)
    hb_meta_info = as.data.table(hb_meta_info)
    tuner_info = tuner$info[, c(1:3, 5)]

    expect_equal(hb_meta_info, tuner_info)
    expect_equal(real_evals, inst$n_evals)
    expect_data_table(bmr$data, nrows = real_evals)

  } else {

    expect_data_table(bmr$data, min.rows = term_evals)
    expect_gte(inst$n_evals, term_evals)
  }

  r = inst$result
  sc = r$tune_x
  sp = r$perf
  expect_list(sc)
  expect_names(names(sc), subset.of = c("xx", "yy", "cp", "nrounds"))
  expect_numeric(sp, len = 1L)
  expect_names(names(sp), identical.to = "regr.mse")
  list(tuner = tuner, inst = inst)
}


# create a simple inst object for rpart with cp param and 2CV resampling
TEST_MAKE_PS1 = function(n_dim = 1L) {
  if (n_dim == 1) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3)
    ))
  } else if (n_dim == 2) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3),
      ParamInt$new("minsplit", lower = 1, upper = 9)
    ))
  }
}
TEST_MAKE_INST1 = function(values = NULL, folds = 2L, measures = msr("classif.ce"), n_dim = 1L, term_evals = 5L) {
  ps = TEST_MAKE_PS1(n_dim = n_dim)
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values)) {
    lrn$param_set$values = values
  }
  rs = rsmp("cv", folds = folds)
  term = term("evals", n_evals = term_evals)
  inst = TuningInstance$new(tsk("iris"), lrn, rs, measures, ps, term)
  return(inst)
}

# create inst object with dependencies
TEST_MAKE_PS2 = function() {
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
TEST_MAKE_INST2 = function(measures = msr("dummy.cp.regr"), term_evals = 5L) {
  ps = TEST_MAKE_PS2()
  ll = LearnerRegrDepParams$new()
  rs = rsmp("holdout")
  term = term("evals", n_evals = term_evals)
  inst = TuningInstance$new(tsk("boston_housing"), ll, rs, measures, ps, term)
  return(inst)
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
      },

      score_internal = function(prediction, learner, ...) {
        self$fun(learner$param_set$values$cp)
      }
    )
  )
}
MeasureDummyCPClassif = make_dummy_cp_measure("classif")
mlr3::mlr_measures$add("dummy.cp.classif", MeasureDummyCPClassif)
MeasureDummyCPRegr = make_dummy_cp_measure("regr")
mlr3::mlr_measures$add("dummy.cp.regr", MeasureDummyCPRegr)

LearnerRegrDepParams = R6Class("LearnerRegrDepParams", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.depparams") {
      param_set = TEST_MAKE_PS2()
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("missings")
      )
    },

    train_internal = function(task) {
      tn = task$target_names
      return(list())
    },

    predict_internal = function(task) {
      n = task$nrow
      response = rep(99, n)
      PredictionRegr$new(task, response = response)
    }
  )
)

