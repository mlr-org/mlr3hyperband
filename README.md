
# mlr3hyperband

Package website: [release](https://mlr3hyperband.mlr-org.com/) |
[dev](https://mlr3hyperband.mlr-org.com/dev/)

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3hyperband/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3hyperband/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

This package provides hyperband tuning for
[`mlr3`](https://mlr3.mlr-org.com).

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3hyperband")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3hyperband")
```

## Resources

  - mlr3book chapter on
    [hyperband](https://mlr3book.mlr-org.com/optimization.html#hyperband)
    and [hyperparameter
    tuning](https://mlr3book.mlr-org.com/optimization.html#tuning).
  - The original [paper](https://arxiv.org/abs/1603.06560) introducing
    the hyperband algorithm.

## Hyperband

Hyperband is a budget oriented-procedure, weeding out suboptimally
performing configurations early on during their training process aiming
at increasing the efficiency of the tuning procedure. For this, several
brackets are constructed with an associated set of configurations for
each bracket. These configuration are initialized by stochastic, often
uniform, sampling. Each bracket is divided into multiple stages, and
configurations are evaluated for a increasing budget in each stage. Note
that currently all configurations are trained completely from the
beginning, so no online updates to the models are performed.

Different brackets are initialized with different number of
configurations, and different budget sizes. To identify the budget for
evaluating hyperband, the user has to specify explicitly which
hyperparameter of the learner influences the budget by tagging a single
hyperparameter in the parameter set with `"budget"`. An alternative
approach using subsampling and pipelines is described further below.

## Examples

### Basic

If you are already familiar with `mlr3tuning`, then the only change
compared to other tuners is to give a numeric hyperparameter a `budget`
tag. Afterwards, you can handle hyperband like all other tuners.
Originally, hyperband was created with a “natural” learning parameter as
the budget parameter in mind, like `nrounds` of the XGBoost learner.

``` r
library(mlr3verse)
library(mlr3hyperband)
library(mlr3learners)

# define hyperparameter and budget parameter
search_space = ps(
  nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
  eta = p_dbl(lower = 0, upper = 1),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "hyperband",
  task = tsk("pima"),
  learner = lrn("classif.xgboost", eval_metric = "logloss"),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  search_space = search_space
)

# best performing hyperparameter configuration
instance$result
```

    ##    nrounds     eta booster learner_param_vals  x_domain classif.ce
    ## 1:       4 0.27844  gbtree          <list[6]> <list[3]>  0.2682292

### Subsampling

Additionally, it is also possible to use `mlr3hyperband` to tune
learners that do not have a natural fidelity parameter. In such a case
`mlr3pipelines` can be used to define data subsampling as a
preprocessing step. Then, the `frac` parameter of subsampling, defining
the fraction of the training data to be used, can act as the budget
parameter.

``` r
learner = po("subsample") %>>% lrn("classif.rpart")

# define subsampling parameter as budget
search_space = ps(
  classif.rpart.cp = p_dbl(lower = 0.001, upper = 0.1),
  classif.rpart.minsplit = p_int(lower = 1, upper = 10),
  subsample.frac = p_dbl(lower = 0.1, upper = 1, tags = "budget")
)

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "hyperband",
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  search_space = search_space
)

# best performing hyperparameter configuration
instance$result
```

    ##    classif.rpart.cp classif.rpart.minsplit subsample.frac learner_param_vals  x_domain classif.ce
    ## 1:        0.0246659                      5            0.5          <list[6]> <list[3]>  0.2395833

### Successive Halving

``` r
library(mlr3hyperband)
library(mlr3learners)

# define hyperparameter and budget parameter
search_space = ps(
  nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
  eta = p_dbl(lower = 0, upper = 1),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "successive_halving",
  task = tsk("pima"),
  learner = lrn("classif.xgboost", eval_metric = "logloss"),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  search_space = search_space
)

# best performing hyperparameter configuration
instance$result
```

    ##    nrounds       eta booster learner_param_vals  x_domain classif.ce
    ## 1:       2 0.8726027    dart          <list[6]> <list[3]>  0.2265625
