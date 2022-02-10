
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

`mlr3hyperband` extends the
[mlr3tuning](https://mlr3tuning.mlr-org.com/) package with multifidelity
optimization methods based on the successive halving algorithm. It
currently provides the following optimizers for
[bbotk](https://bbotk.mlr-org.com/) and tuner for
[mlr3tuning](https://mlr3tuning.mlr-org.com/):

  - Successive Halving (`OptimizerSuccessiveHalving` &
    `TunerSuccessiveHalving`)
  - Hyperband (`OptimizerSuccessiveHalving` & `TunerSuccessiveHalving`)

## Resources

  - mlr3book chapter on
    [hyperband](https://mlr3book.mlr-org.com/optimization.html#hyperband)
    and [hyperparameter
    tuning](https://mlr3book.mlr-org.com/optimization.html#tuning).
  - The original publications introducing the [successive
    halving](https://arxiv.org/abs/1502.07943) and
    [hyperband](https://arxiv.org/abs/1603.06560).
  - Ask questions on [Stackoverflow (tag
    \#mlr3)](https://stackoverflow.com/questions/tagged/mlr3)

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3hyperband")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3hyperband")
```

## Examples

### Basic

``` r
library(mlr3hyperband)
library(mlr3learners)

# load learner, define search space and tag budget hyperparameter
learner = lrn("classif.xgboost",
  nrounds           = to_tune(p_int(27, 243, tags = "budget")),
  eta               = to_tune(1e-4, 1, logscale = TRUE),
  max_depth         = to_tune(1, 20),
  colsample_bytree  = to_tune(1e-1, 1),
  colsample_bylevel = to_tune(1e-1, 1),
  lambda            = to_tune(1e-3, 1e3, logscale = TRUE),
  alpha             = to_tune(1e-3, 1e3, logscale = TRUE),
  subsample         = to_tune(1e-1, 1)
)

# set parallel backend
future::plan("multisession")

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "hyperband",
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  eta = 3
)
```

### Subsample

``` r
library(mlr3hyperband)
library(mlr3learners)
library(mlr3pipelines)

# load learner and define search space
learner = lrn("classif.rpart",
  minsplit  = to_tune(2, 128, logscale = TRUE),
  minbucket = to_tune(1, 64, logscale = TRUE),
  cp        = to_tune(1e-04, 1e-1, logscale = TRUE)
)

# create graph learner with subsampling
graph_learner = as_learner(po("subsample") %>>% learner)

# tag budget parameter
graph_learner$param_set$values$subsample.frac = to_tune(p_dbl(3^-3, 1, tags = "budget"))

# set parallel backend
future::plan("multisession")

# hyperparameter tuning on the spam data set
instance = tune(
  method = "hyperband",
  task = tsk("spam"),
  learner = graph_learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  eta = 3
)
```

### Quick general-purpose optimization

``` r
library(bbotk)
library(mlr3hyperband)

# define hyperparameter and budget parameter
search_space = domain = ps(
  x1        = p_dbl(-5, 10),
  x2        = p_dbl(0, 15),
  fidelity  = p_dbl(1e-2, 1, tags = "budget")
)

# modified branin function
objective = ObjectiveRFun$new(
  fun = branin,
  domain = domain,
  codomain = ps(y = p_dbl(tags = "minimize"))
)

# optimize branin function with hyperband
result = bb_optimize(objective, method = "hyperband", search_space = search_space, term_evals = NULL, eta = 2)

# optimized parameters
result$par
```

    ##           x1       x2 fidelity
    ## 1: -3.323411 11.43229   0.0625

``` r
# optimal outcome
result$value
```

    ##         y 
    ## 0.6178947
