# mlr3hyperband

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mlr3hyperband/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3hyperband/actions)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/mlr3hyperband)](https://cran.rstudio.com/web/packages/mlr3hyperband/index.html)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![codecov](https://codecov.io/gh/mlr-org/mlr3hyperband/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3hyperband)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
<!-- badges: end -->

This package provides hyperband tuning for [mlr3](https://mlr3.mlr-org.com.
Various termination criteria can be set and combined. The class 'AutoTuner'
provides a convenient way to perform nested resampling in combination with
'mlr3'.

## Installation

Development version

``` r
remotes::install_github("mlr-org/mlr3hyperband")
```

## Quickstart

If you are already familiar with `mlr3tuning`, then the only change
compared to other tuners is to give a numeric hyperparameter a
`budget` tag. Afterwards, you can handle hyperband like all other
tuners:

``` r
library(paradox)
library(mlr3tuning)
library(mlr3hyperband)

# give a hyperparameter the "budget" tag
params = list(
  ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)

inst = ...  # here goes the usual mlr3tuning TuningInstance constructor

# initialize hyperband tuner
tuner = tnr("hyperband", eta = 2L)

# tune the previously defined TuningInstance
tuner$optimize(inst)
```

For the full working example, please check out the Examples section
below.

## A short description of hyperband

Hyperband is a budget oriented-procedure, weeding out suboptimally
performing configurations early on during their training process aiming
at increasing the efficiency of the tuning procedure. For this, several
brackets are constructed with an associated set of configurations for
each bracket. These configuration are initialized by stochastic, often
uniform, sampling. Each bracket is divided into multiple stages, and
configurations are evaluated for a increasing budget in each stage. Note
that currently all configurations are trained completely from the
beginning, so no online updates to the models are performed.

Different brackets are initialized with different number of configurations, and
different budget sizes. To identify the budget for evaluating hyperband, the
user has to specify explicitly which hyperparameter of the learner influences
the budget by tagging a single hyperparameter in the parameter set with
`"budget"`. An alternative approach using subsampling and pipelines is described
further below.

## Examples

Originally, hyperband was created with a “natural” learning parameter as
the budget parameter in mind, like `nrounds` of the XGBoost learner:

``` r
library(mlr3)
library(mlr3hyperband) # hyperband tuner
library(mlr3tuning) # tuning methods
library(mlr3learners) # xgboost learner
library(paradox) # search space definition
set.seed(123)

# Define hyperparameter and budget parameter for tuning with hyperband
params = list(
  ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)

# Initialize TuningInstance as usual
# hyperband terminates on its own, so the terminator acts as a upper bound
inst = TuningInstanceSingleCrit$new(
  task = tsk("iris"),
  learner = lrn("classif.xgboost"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = ParamSet$new(params),
  terminator = trm("none") # hyperband terminates on its own
)

# Initialize Hyperband Tuner and tune
tuner = tnr("hyperband", eta = 2L)
tuner$optimize(inst)

# View results
inst$result
```

Additionally, it is also possible to use `mlr3hyperband` to tune
learners that do not have a natural fidelity parameter. In such a case
`mlr3pipelines` can be used to define data subsampling as a
preprocessing step. Then, the `frac` parameter of subsampling, defining
the fraction of the training data to be used, can act as the budget
parameter:

``` r
library(mlr3pipelines)
set.seed(123)

ll = po("subsample") %>>% lrn("classif.rpart")

# Define extended hyperparameters with subsampling fraction as budget and hence
# no learner budget is required
params = list(
  ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
  ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
  ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
)

# Define TuningInstance with the Graph Learner and the extended hyperparams
inst = TuningInstanceSingleCrit$new(
  tsk("iris"),
  ll,
  rsmp("holdout"),
  msr("classif.ce"),
  ParamSet$new(params),
  trm("none") # hyperband terminates on its own
)

# Initialize Hyperband Tuner and tune
tuner = tnr("hyperband", eta = 4L)
tuner$optimize(inst)

# View results
inst$result
```

## Documentation

The function reference is can be found
[here](https://mlr3hyperband.mlr-org.com/reference/). Further
documentation lives in the [mlr3book](https://mlr3book.mlr-org.com/).

The original paper introducing the hyperband algorithm is given
[here](https://arxiv.org/abs/1603.06560).
