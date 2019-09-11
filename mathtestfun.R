load_all()
library(mlr3)
library(globalOptTests)
# options(warn = 2)
set.seed(123)
# test(filter = "Neld")
lg$set_threshold("info")

# param_set = ParamSet$new(
#   params = list(
#     ParamDbl$new("cp", lower = 0.1, upper = 1),
#     ParamInt$new("maxdepth",  lower = 1, upper = 3)
#   )
# )

# terminator = TerminatorEvals$new(100000)

# rr = ResamplingHB$new()

# inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"),
#   rr, msr("classif.ce"), param_set, terminator)
# tuner = TunerHB$new(eta = 3, config_max_budget = 81)
# tuner$tune(inst)
# print(inst$archive())
# print(tuner$info)


# ii = tuner$info
# print(ii[, .(
#       bmin = min(budget), bmax = max(budget),
#       bracket_stages = max(bracket_stage)),
#   by = bracket])


############################################



MeasureMathTestFun = R6Class("MeasureMathTestfun",
  inherit = MeasureRegr,
  public = list(
    tfun = NULL,

    initialize = function(tfun) {
      self$tfun = tfun
      super$initialize(
        id = "dmtf",
        range = c(-Inf, Inf),
        minimize = TRUE
      )
    },

    score_internal = function(prediction, learner, ...) {
      v = learner$param_set$values
      v$budget = NULL
      goTest(unlist(v), tfun)
    }
  )
)

LearnerMathTestFun = R6Class(
  "LearnerMathTestFun",
  inherit = LearnerRegr,

  public = list(
    initialize = function(tfun) {
      b = getDefaultBounds(tfun)
      k = getProblemDimen(tfun)
      ps = Map(
        function(i) ParamDbl$new(paste0("x", i), lower = b$lower[i], upper = b$upper[i]),
        1:k
      )
      ps = ParamSet$new(ps)
      ps$add(ParamDbl$new("budget", lower = 0, upper = 100))
      super$initialize(
        id = "regr.mtf",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        properties = c("missings")
      )
    },

    train_internal = function(task) {
      n = length(task$row_roles$use)
      self$param_set$values$budget = n / length(task$row_ids)
      return(list())
    },

    predict_internal = function(task) {
      response = rep(99, task$nrow)
      PredictionRegr$new(task, response = response)
    }
  )
)

terminator = TerminatorEvals$new(20)

# TODO: multi crit with ZDT and DTLZ family
tfun = "Branin"
mm = MeasureMathTestFun$new(tfun)
ll = LearnerMathTestFun$new(tfun)

inst = TuningInstance$new(tsk("boston_housing"), ll,
  rsmp("holdout"), mm, ll$param_set, terminator)

tuner = TunerRandomSearch$new()
tuner$tune(inst)
print(inst$archive())


