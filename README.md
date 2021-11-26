
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

### Successive Halving

Successive halving starts with minimum budget and a given, fixed number
of `n` candidates and races them down in stages to a single best
candidate by repeatedly evaluating all candidates with increased budgets
in a certain schedule. The budget is increased by a factor of `eta` and
only the best `1 / eta` fraction of candidates is promoted to the next
stage. The budget hyperparameter must be tagged with `"budget"` in the
search space. The minimum and maximum budget is set by the lower and
upper bound of the budget parameter.

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
    ## 1:       4 0.3668981  gbtree          <list[6]> <list[3]>  0.2695312

### Hyperband

Hyperband repeatedly calls successive halving with different numbers of
starting configurations. Each run of SH within hyperband is called a
bracket. All brackets spend approximately the same budget i.e. a larger
number of starting configurations corresponds to a smaller budget
allocated in the base stage. Use `hyperband_schedule()` to get a preview
of the bracket layout.

| s |    |  3 |  |    |  2 |  |    |  1 |  |    |  0 |
| -: | -: | -: |  | -: | -: |  | -: | -: |  | -: | -: |
| i | ni | ri |  | ni | ri |  | ni | ri |  | ni | ri |
| 0 |  8 |  1 |  |  6 |  2 |  |  4 |  4 |  |  8 |  4 |
| 1 |  4 |  2 |  |  3 |  4 |  |  2 |  8 |  |    |    |
| 2 |  2 |  4 |  |  1 |  8 |  |    |    |  |    |    |
| 3 |  1 |  8 |  |    |    |  |    |    |  |    |    |

The budget hyperparameter must be tagged with `"budget"` in the search
space. The minimum and maximum budget is set by the lower and upper
bound of the budget parameter. An alternative approach using subsampling
is described further below.

If you are already familiar with `mlr3tuning`, then the only change
compared to other tuners is to give a numeric hyperparameter a
`"budget"` tag. Afterwards, you can handle hyperband like all other
tuners. Originally, hyperband was created with a “natural” learning
parameter as the budget parameter in mind, like `nrounds` of the XGBoost
learner.

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

    ##    nrounds       eta booster learner_param_vals  x_domain classif.ce
    ## 1:       8 0.2462847    dart          <list[6]> <list[3]>  0.2356771

### Hyperband and Subsampling

It is also possible to tune learners that do not have a natural fidelity
parameter. In such a case `mlr3pipelines` can be used to define data
subsampling as a preprocessing step. Then, the `frac` parameter of
subsampling, defining the fraction of the training data to be used, can
act as the budget parameter.

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
    ## 1:       0.05666602                     10           0.25          <list[6]> <list[3]>  0.2486979

### Asynchronous Successive Halving

Asynchronous successive halving (ASHA) parallelizes SHA by promoting
candidates to the next stage as soon as possible instead of waiting for
all candidates in the stage to finish. ASHA starts with sampling a
candidate points for each available worker. When an evaluation finishes
and the worker is available again, ASHA checks the stages from top to
bottom for promotable candidates. Promotions are possible when the
evaluated candidates belong to the top `1 / eta` of each stage. If no
promotions are possible, a new candidate is sampled and added to the
base stage, which increases the number of possible promotions for all
stages. Asha needs a parallel backend which is supplied with
`future::plan()`.

``` r
library(mlr3hyperband)
library(mlr3learners)

future::plan("multisession")

# define hyperparameter and budget parameter
search_space = ps(
  nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
  eta = p_dbl(lower = 0, upper = 1),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "asha",
  task = tsk("pima"),
  learner = lrn("classif.xgboost", eval_metric = "logloss"),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  search_space = search_space,
  term_evals = 100
)
```

    ## INFO  [09:39:11.208] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:12.215] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:12.380] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:11.608] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:12.838] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:13.037] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:11.983] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:13.343] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:13.537] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:12.691] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:14.038] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:14.224] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:13.433] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:14.859] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:15.100] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:13.964] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:15.355] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:15.652] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:15.437] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:15.683] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:16.032] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:15.632] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:16.075] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:16.423] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:16.318] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:16.696] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:17.152] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:14.617] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:16.904] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:17.466] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:15.154] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:17.621] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:18.080] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:16.598] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:17.017] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:17.460] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:17.251] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:17.736] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:18.144] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:17.571] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:18.037] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:18.492] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:18.240] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:18.606] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:18.924] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:18.717] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:19.064] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:19.375] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:19.077] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:19.376] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:19.584] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:19.673] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:19.979] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:20.308] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:19.829] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:20.084] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:20.352] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:20.137] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:20.464] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:20.854] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:20.692] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:21.036] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:21.468] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:20.755] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:21.061] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:21.465] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:21.133] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:21.424] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:21.772] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:21.634] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:21.933] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:22.229] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:22.074] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:22.332] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:22.558] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:22.554] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:22.804] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:23.152] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:22.780] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:23.171] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:23.425] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:22.895] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:23.136] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:23.463] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:23.594] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:23.928] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:24.209] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:23.871] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:24.180] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:24.537] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:24.007] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:24.383] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:24.803] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:24.277] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:24.652] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:25.131] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:24.765] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:25.084] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:25.489] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:25.313] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:25.744] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:26.024] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:25.490] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:25.837] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:26.072] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:26.262] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:26.514] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:26.776] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:26.415] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:26.626] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:26.837] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:27.042] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:27.251] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:27.538] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:27.133] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:27.357] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:27.563] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:27.927] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:28.147] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:28.355] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:28.003] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:28.190] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:28.416] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:28.823] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:29.139] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:29.471] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:28.578] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:28.804] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:29.237] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:28.827] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:29.165] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:29.504] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:29.308] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:29.741] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:30.165] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:29.524] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:29.962] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:30.264] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:29.896] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:30.338] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:30.667] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:30.626] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:30.875] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:31.136] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:30.808] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:31.132] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:31.453] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:31.205] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:31.487] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:31.821] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:31.739] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:32.010] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:32.444] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:32.005] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:32.397] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:32.773] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:32.212] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:32.509] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:32.869] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:32.760] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:33.131] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:33.275] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:33.489] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:33.807] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:34.175] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:33.608] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:33.892] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:34.169] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:33.710] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:33.993] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:34.349] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:34.266] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:34.621] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:34.864] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:34.868] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:35.111] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:35.273] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:35.309] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:35.554] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:35.771] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:35.550] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:35.846] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:36.230] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:35.712] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:36.020] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:36.256] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:36.534] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:36.904] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:37.320] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:36.634] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:36.938] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:37.299] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:36.759] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:37.123] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:37.616] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:37.086] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:37.561] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:37.987] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:37.896] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:38.419] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:38.974] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:38.134] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:38.479] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:38.829] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:38.283] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:38.619] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:39.056] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:38.861] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:39.250] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:39.612] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:39.477] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:39.930] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:40.361] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:39.710] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:40.151] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:40.632] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:40.362] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:40.782] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:41.127] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:40.227] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:40.779] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:41.173] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:41.654] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:41.969] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:42.321] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:41.851] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:42.173] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:42.616] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:42.209] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:42.704] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:43.170] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:41.299] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:41.769] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:42.210] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:42.706] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:43.212] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:43.766] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:43.113] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:43.494] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:44.081] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:43.429] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:44.153] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:44.589] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:43.923] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:44.481] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:45.049] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:44.943] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:45.306] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:45.650] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:45.224] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:45.562] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:45.864] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:44.548] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:44.997] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:45.505] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:46.120] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:46.400] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:46.692] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:46.186] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:46.457] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:46.757] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:46.470] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:46.800] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:47.122] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:47.148] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:47.446] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:47.813] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:47.360] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:47.840] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:48.197] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:47.766] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:48.152] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:48.478] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:47.772] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:48.090] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:48.450] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:48.827] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:49.073] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:49.396] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:48.924] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:49.286] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:49.705] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:49.033] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:49.353] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:49.860] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:49.519] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:50.002] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:50.219] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:50.280] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:50.707] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:51.094] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:50.484] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:50.830] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:51.200] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:51.148] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3) 
    ## INFO  [09:39:51.361] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:51.643] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:51.712] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 1/3) 
    ## INFO  [09:39:52.038] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 2/3) 
    ## INFO  [09:39:52.219] [mlr3] Applying learner 'classif.xgboost' on task 'pima' (iter 3/3)

``` r
# best performing hyperparameter configuration
instance$result
```

    ##    nrounds       eta booster learner_param_vals  x_domain classif.ce
    ## 1:       8 0.3896997    dart          <list[6]> <list[3]>  0.2291667
