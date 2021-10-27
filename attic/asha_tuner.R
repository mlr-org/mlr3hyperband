library(mlr3tuningspaces)
library(mlr3learners)
#devtools::load_all("../bbotk")
#devtools::load_all("../mlr3tuning")
devtools::load_all(".")

library(future)
plan(multicore)

set.seed(7832)

task = tsk("pima")
learner = lrn("classif.xgboost",
  eta = to_tune(1e-4, 1, logscale = TRUE),
  nrounds = to_tune(p_int(1, 256, tags = "budget")),
  max_depth = to_tune(1, 20),
  eval_metric = "logloss")

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 30)
)

tuner = TunerAsha$new()

tuner$optimize(instance)
