# mlr3hyperband

Extends the [mlr3](https://mlr3.mlr-org.com) package with hyperband tuning.

[![Build Status](https://travis-ci.org/mlr-org/mlr3hyperband.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3hyperband)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version-ago/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)
[![cran checks](https://cranchecks.info/badges/worst/mlr3hyperband)](https://cran.r-project.org/web/checks/check_results_mlr3hyperband.html)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mlr3hyperband)](https://cran.rstudio.com/web/packages/mlr3hyperband/index.html)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![codecov](https://codecov.io/gh/mlr-org/mlr3hyperband/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3hyperband)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Dependencies](https://tinyverse.netlify.com/badge/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)

## Installation

```r
remotes::install_github("mlr-org/mlr3hyperband")
```


## Quickstart


If you are already familiar with `mlr3tuning`, then the only change compared to other tuners is to give a numeric hyperparameter a `"budget"` tag.
Afterwards, you can handle hyperband like all other tuners:

```
library(mlr3hyperband)

# give a hyperparameter the "budget" tag
params = list(
  ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)

#inst = ... here goes the usual mlr3tuning TuningInstance constructor

# initialize hyperband tuner 
tuner = TunerHyperband$new(eta = 2L)

# tune the previously defined TuningInstance
#tuner$tune(inst)
```

For the full working example, please check out the `Examples` section below.


## Using Hyperband in mlr3

Hyperband is a budget oriented-procedure, weeding out suboptimally performing configurations early on during their training process, increasing tuning efficiency as a consequence.
For this, several brackets are constructed with an associated set of configurations for each bracket. These configuration are initialized by stochastic, often uniform, sampling.
Each bracket is divided into multiple stages, and configurations are evaluated for a increasing budget in each stage.
Note that currently all configurations are trained completely from the beginning, so no online updates of models.

Different brackets are initialized with different number of configurations, and different budget sizes.

To identify the budget for evaluating hyperband, the user has to specify explicitly which hyperparameter of the learner influences the budget by tagging a single hyperparameter in the [paradox::ParamSet] with `"budget"`.
An alternative approach using subsampling and pipelines is described below.


## Examples

Originally, hyperband was created with a "natural" fidelity parameter of the learner as the budget parameter, like `nrounds` of the XGBoost learner:

```
library(mlr3hyperband)
library(mlr3learners)
set.seed(123)

# define hyperparameter and budget parameter for tuning with hyperband
params = list(
  ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)

# initialize TuningInstance as usual
# hyperband terminates on its own, so the terminator acts as a upper bound
inst = TuningInstance$new(
  task = tsk("iris"),
  learner = lrn("classif.xgboost"),
  resampling = rsmp("holdout"),
  measures = msr("classif.ce"),
  ParamSet$new(params),
  term("evals", n_evals = 100000L) # high value to let hyperband finish
)

# initialize Hyperband Tuner and tune
tuner = TunerHyperband$new(eta = 2L)
tuner$tune(inst)

# return best result
inst$best()
```

Additionally, our framework also supports the case when no natural fidelity parameter is given by the learner.
In this case, one can use `mlr3pipelines` to define subsampling as a preprocessing step.
Then, the `frac` parameter of subsampling, defining the fraction of the training data to be used, can act as the budget parameter:

```
library(mlr3hyperband)
library(mlr3pipelines)
set.seed(123)

ll = po("subsample") %>>% lrn("classif.rpart")

# define extended hyperparameters with subsampling fraction as budget
# ==> no learner budget is required
params = list(
  ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
  ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
  ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
)

# define TuningInstance with the Graph Learner and the extended hyperparams
inst = TuningInstance$new(
  tsk("iris"),
  ll,
  rsmp("holdout"),
  msr("classif.ce"),
  ParamSet$new(params),
  term("evals", n_evals = 100000L) # high value to let hyperband finish
)

tuner = TunerHyperband$new(eta = 4L)
tuner$tune(inst)

# return best result
inst$best()
```


## Documentation

The function reference is can be found [here](https://mlr3hyperband.mlr-org.com/reference/).
Further documentation lives in the [mlr3book](https://mlr3book.mlr-org.com/).
The original paper introducing the hyperband algorithm is given [here](https://arxiv.org/abs/1603.06560). 
