devtools::load_all(".")
library(mlr3learners)

# define hyperparameter and budget parameter
search_space = ps(
  nrounds = p_int(lower = 2, upper = 32, tags = "budget"),
  eta = p_dbl(lower = 0, upper = 1),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)

instance_hyperstage = TuningInstanceSingleCrit$new(
  task = tsk("pima"),
  learner = lrn("classif.xgboost", eval_metric = "logloss"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = trm("none"),
  search_space = search_space
)

tuner = TunerHyperStage$new()
tuner$param_set$values$eta = 2

tuner$optimize(instance_hyperstage)

# instance_hyperstage = readRDS("attic/instance_hyperstage.rda")    

archive = as.data.table(instance_hyperstage$archive)
set(archive, j = "time", value = archive$timestamp - instance_hyperstage$archive$start_time)

library(ggplot2)

ggplot(archive, aes(x = time, y = nrounds)) +
  stat_summary(geom = "col", fun = mean)
