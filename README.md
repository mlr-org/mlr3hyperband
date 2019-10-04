# mlr3hyperband

[![Build Status](https://travis-ci.org/mlr-org/mlr3hyperband.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3hyperband)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version-ago/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)
[![cran checks](https://cranchecks.info/badges/worst/mlr3hyperband)](https://cran.r-project.org/web/checks/check_results_mlr3hyperband.html)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlr3hyperband)](https://cran.rstudio.com/web/packages/mlr3hyperband/index.html)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![codecov](https://codecov.io/gh/mlr-org/mlr3hyperband/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3hyperband)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)

Extends the [mlr3](https://mlr3.mlr-org.com) package with hyperband tuning.

## Installation

```r
remotes::install_github("mlr-org/mlr3hyperband")
```

## Using Hyperband in mlr3

Hyperband is a budget oriented procedure putting more ressources on more promising configurations, increasing tuning efficiency as a consequence.
For this, several brackets are constructed with different starting configurations in each.
Each bracket has a different amount of stages depending with a different starting budget -- in general the more stages the lower the budget at first.
Once a stage of a bracket is evaluated, the best `1/eta` configurations are kept, while the rest is discarded.
The remaining configurations are then transfered to the next bracket stage, where training is continued with an increase of the budget by the factor of `eta`.
This continuous iteratively for every bracket stage until the upper limit of the budget is reached.
In the end, and aggregated over all brackets, we have a lot of evaluated configurations with only a small handful being trained on the upper limit of the budget.
This safes a lot of training time on configurations, that look unpromising on a low budget, as they are skipped for further evaluation.
There are currently two ways to identify the budget during tuning.
One is by using the size of the training set as the budget, with the full set as the maximum budget:

```
library(mlr3hyperband)
set.seed(123)

# define hyperparameter for tuning with hyperband
params = list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 10)
)

# initialize TuningInstance as usual
inst = TuningInstance$new(
  task = tsk("iris"),
  learner = lrn("classif.rpart"),
  resampling = rsmp("holdout"),
  measures = msr("classif.ce"),
  ParamSet$new(params),
  term("evals", n_evals = 100000L) # hyperband stops on its own
)

# initialize Hyperband Tuner and tune
tuner = TunerHyperband$new(eta = 2L, use_subsampling = TRUE)
result = tuner$tune(inst)

# view results
instance$archive()[, c("cp", "minsplit", "classif.ce")]
```

While the other way is by explicitly specifying which learner's hyperparameter is the budget (here, we use XGBoost with `nrounds` as budget parameter):

```
library(mlr3learners)
set.seed(123)

# define hyperparameter and budget parameter for tuning with hyperband
params = list(
  ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)

# initialize TuningInstance as usual
inst = TuningInstance$new(
  task = tsk("iris"),
  learner = lrn("classif.xgboost"),
  resampling = rsmp("holdout"),
  measures = msr("classif.ce"),
  ParamSet$new(params),
  term("evals", n_evals = 100000L)
)

# initialize Hyperband Tuner and tune
tuner = TunerHyperband$new(eta = 2L)
result = tuner$tune(inst)

# view results
instance$archive()[, c("nrounds", "eta", "booster", "classif.ce")]
```


## Documentation

The function reference is can be found [here](https://mlr3hyperband.mlr-org.com/reference/).
Further documentation lives in the [mlr3book](https://mlr3book.mlr-org.com/).
 
