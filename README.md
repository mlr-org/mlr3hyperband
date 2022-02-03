
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
[mlr3tuning](https://mlr3tuning.mlr-org.com/) package with various
multifidelity optimization methods based on successive halving
algorithm. It currently provides the following optimizers for
[bbotk](https://bbotk.mlr-org.com/) and tuner for
[mlr3tuning](https://mlr3tuning.mlr-org.com/):

  - Successive Halving (`OptimizerSuccessiveHalving` &
    `TunerSuccessiveHalving`)
  - Asynchronous Successive Halving (`OptimizerAsha` & `TunerAsha`)
  - Hyperband (`OptimizerSuccessiveHalving` & `TunerSuccessiveHalving`)
  - Asynchronous Hyperband (`OptimizerAhb` & `TunerAhb`)

## Resources

  - mlr3book chapter on
    [hyperband](https://mlr3book.mlr-org.com/optimization.html#hyperband)
    and [hyperparameter
    tuning](https://mlr3book.mlr-org.com/optimization.html#tuning).
  - The original publications introducing the [successive
    halving](https://arxiv.org/abs/1502.07943),
    [hyperband](https://arxiv.org/abs/1603.06560) and
    [asha](https://arxiv.org/abs/1810.05934) algorithm.
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

``` r
library(mlr3hyperband)
library(mlr3learners)

# load learner and define search space
learner = lrn("classif.xgboost",
  nrounds = to_tune(p_int(lower = 1, upper = 16, tags = "budget")),
  eta = to_tune(0, 1),
  booster = to_tune()
)

# set parallel backend
future::plan("multisession")

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "asha",
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  term_evals = 100
)
```
<<<<<<< HEAD
=======

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

### Quick general-purpose optimization

``` r
library(bbotk)
library(mlr3hyperband)

# define hyperparameter and budget parameter
search_space = domain = ps(
  x1 = p_dbl(-5, 10),
  x2 = p_dbl(0, 15),
  fidelity = p_dbl(1e-2, 1, tags = "budget")
)

# modified branin function
objective = ObjectiveRFun$new(
  fun = branin,
  domain = domain,
  codomain = ps(y = p_dbl(tags = "minimize"))
)

# optimize branin function with hyperband
result = bb_optimize(objective, method = "hyperband", search_space = search_space, term_evals = NULL)

# optimized parameters
result$par
```

    ##          x1       x2 fidelity
    ## 1: 9.739074 2.508206        1

``` r
# optimal outcome
result$value
```

    ##         y 
    ## 0.9281165
>>>>>>> main
