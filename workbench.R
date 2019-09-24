devtools::load_all()
library(mlr3learners)
library(mlr3pipelines)

set.seed(123)

# define hyperparameter and budget parameter for tuning with hyperband
params = list(
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)


inst = TuningInstance$new(
  tsk("iris"),
  lrn("classif.xgboost"),
  rsmp("holdout"),
  msr("classif.ce"),
  ParamSet$new(params),
  term("evals", n_evals = 100000)
)


tuner = TunerHyperband$new(eta = 2L, use_subsamp = TRUE)
tuner$tune(inst)

print(inst$archive())
print(tuner$info)

# sanity check of brackets layout
tuner$info[, .(
  bracket_stages = max(bracket_stage),
  b_start = min(budget),
  b_end = max(budget),
  b_sum = sum(budget * mu),
  b_real_start = min(budget_real),
  b_real_end = max(budget_real),
  b_real_sum = sum(budget_real * mu),
  mu_start = max(mu),
  mu_end = min(mu)
), by = bracket]



###################################################
# multicrit
###################################################

devtools::load_all()
library(mlr3learners)


set.seed(123)

# define hyperparameter and budget parameter for tuning with hyperband
ps = ParamSet$new(list(

  ParamInt$new("nrounds",           lower = 1, upper = 81, tag = "budget"),
  ParamDbl$new("eta",               lower = 0, upper = 1),
  ParamInt$new("num_parallel_tree", lower = 1, upper = 100),
  ParamInt$new("max_depth",         lower = 1, upper = 100),
  ParamFct$new("normalize_type", levels = c("tree", "forest")),
  ParamFct$new("sample_type",    levels = c("uniform", "weighted")),
  ParamFct$new("booster",        levels = c("gbtree", "gblinear", "dart"))

))


inst = TuningInstance$new(
  tsk("pima"),
  lrn("classif.xgboost"),
  rsmp("holdout"),
  list(msr("classif.tpr"), msr("classif.fpr")),
  ps,
  term("evals", n_evals = 100000)
)

tuner = TunerHyperband$new(eta = 3)
tuner$tune(inst)

print(inst$archive())
print(tuner$info)

# sanity check of brackets layout
tuner$info[, .(
  bracket_stages = max(bracket_stage),
  b_start = min(budget),
  b_end = max(budget),
  b_sum = sum(budget * mu),
  b_real_start = min(budget_real),
  b_real_end = max(budget_real),
  b_real_sum = sum(budget_real * mu),
  mu_start = max(mu),
  mu_end = min(mu)
), by = bracket]


