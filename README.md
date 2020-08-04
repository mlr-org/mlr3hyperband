
# mlr3hyperband

Extends the [mlr3](https://mlr3.mlr-org.com) package with hyperband
tuning.

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

## Installation

Install from github by running the following line:

``` r
remotes::install_github("mlr-org/mlr3hyperband")
```

## Quickstart

If you are already familiar with `mlr3tuning`, then the only change
compared to other tuners is to give a numeric hyperparameter a
`"budget"` tag. Afterwards, you can handle hyperband like all other
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

Different brackets are initialized with different number of
configurations, and different budget sizes.

To identify the budget for evaluating hyperband, the user has to specify
explicitly which hyperparameter of the learner influences the budget by
tagging a single hyperparameter in the parameter set with `"budget"`. An
alternative approach using subsampling and pipelines is described
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

# define hyperparameter and budget parameter for tuning with hyperband
params = list(
  ParamInt$new("nrounds", lower = 1, upper = 16, tags = "budget"),
  ParamDbl$new("eta",     lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
)

# initialize TuningInstance as usual
# hyperband terminates on its own, so the terminator acts as a upper bound
inst = TuningInstanceSingleCrit$new(
  task = tsk("iris"),
  learner = lrn("classif.xgboost"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  search_space = ParamSet$new(params),
  terminator = trm("none") # hyperband terminates on its own
)

# initialize Hyperband Tuner and tune
tuner = tnr("hyperband", eta = 2L)
tuner$optimize(inst)
```

    ## INFO  [16:25:48.217] Starting to optimize 3 parameter(s) with '<TunerHyperband>' and '<TerminatorNone>' 
    ## INFO  [16:25:48.281] Amount of brackets to be evaluated = 5,  
    ## INFO  [16:25:48.291] Start evaluation of bracket 1 
    ## INFO  [16:25:48.297] Training 16 configs with budget of 1 for each 
    ## INFO  [16:25:48.301] Evaluating 16 configuration(s) 
    ## INFO  [16:25:50.782] Result of batch 1: 
    ## INFO  [16:25:50.786]        eta  booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:50.786]  0.2457237 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.7321352 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.8474532     dart       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.4975273   gbtree       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.3879090 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.2464490   gbtree       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.1110965 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.3899944   gbtree       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.5719353     dart       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.2168928     dart       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.4447680     dart       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.2179907 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.5022996 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.3539046 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.6499852     dart       1       4             0             1           1 
    ## INFO  [16:25:50.786]  0.3747140 gblinear       1       4             0             1           1 
    ## INFO  [16:25:50.786]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.04 <ResampleResult> 
    ## INFO  [16:25:50.786]         16       0.74 <ResampleResult> 
    ## INFO  [16:25:50.787] Training 8 configs with budget of 2 for each 
    ## INFO  [16:25:50.790] Evaluating 8 configuration(s) 
    ## INFO  [16:25:51.424] Result of batch 2: 
    ## INFO  [16:25:51.428]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:51.428]  0.8474532    dart       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.4975273  gbtree       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.2464490  gbtree       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.3899944  gbtree       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.5719353    dart       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.2168928    dart       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.4447680    dart       2       4             1             2           2 
    ## INFO  [16:25:51.428]  0.6499852    dart       2       4             1             2           2 
    ## INFO  [16:25:51.428]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.428]          8       0.04 <ResampleResult> 
    ## INFO  [16:25:51.430] Training 4 configs with budget of 4 for each 
    ## INFO  [16:25:51.433] Evaluating 4 configuration(s) 
    ## INFO  [16:25:51.756] Result of batch 3: 
    ## INFO  [16:25:51.759]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:51.759]  0.8474532    dart       4       4             2             4           4 
    ## INFO  [16:25:51.759]  0.4975273  gbtree       4       4             2             4           4 
    ## INFO  [16:25:51.759]  0.2464490  gbtree       4       4             2             4           4 
    ## INFO  [16:25:51.759]  0.3899944  gbtree       4       4             2             4           4 
    ## INFO  [16:25:51.759]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:51.759]          4       0.04 <ResampleResult> 
    ## INFO  [16:25:51.759]          4       0.04 <ResampleResult> 
    ## INFO  [16:25:51.759]          4       0.04 <ResampleResult> 
    ## INFO  [16:25:51.759]          4       0.04 <ResampleResult> 
    ## INFO  [16:25:51.760] Training 2 configs with budget of 8 for each 
    ## INFO  [16:25:51.764] Evaluating 2 configuration(s) 
    ## INFO  [16:25:51.940] Result of batch 4: 
    ## INFO  [16:25:51.943]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:51.943]  0.8474532    dart       8       4             3             8           8 
    ## INFO  [16:25:51.943]  0.4975273  gbtree       8       4             3             8           8 
    ## INFO  [16:25:51.943]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:51.943]          2       0.04 <ResampleResult> 
    ## INFO  [16:25:51.943]          2       0.04 <ResampleResult> 
    ## INFO  [16:25:51.944] Training 1 configs with budget of 16 for each 
    ## INFO  [16:25:51.948] Evaluating 1 configuration(s) 
    ## INFO  [16:25:52.049] Result of batch 5: 
    ## INFO  [16:25:52.052]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:52.052]  0.8474532    dart      16       4             4            16          16 
    ## INFO  [16:25:52.052]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:52.052]          1       0.04 <ResampleResult> 
    ## INFO  [16:25:52.053] Start evaluation of bracket 2 
    ## INFO  [16:25:52.057] Training 10 configs with budget of 2 for each 
    ## INFO  [16:25:52.060] Evaluating 10 configuration(s) 
    ## INFO  [16:25:52.822] Result of batch 6: 
    ## INFO  [16:25:52.825]        eta  booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:52.825]  0.8397678   gbtree       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.3124482   gbtree       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.7082903     dart       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.2650178 gblinear       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.5943432     dart       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.4812898 gblinear       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.2650327 gblinear       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.5645904 gblinear       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.9131882   gbtree       2       3             0             2           2 
    ## INFO  [16:25:52.825]  0.9018744 gblinear       2       3             0             2           2 
    ## INFO  [16:25:52.825]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:52.825]         10       0.04 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.04 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.04 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.74 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.04 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.44 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.52 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.42 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.04 <ResampleResult> 
    ## INFO  [16:25:52.825]         10       0.44 <ResampleResult> 
    ## INFO  [16:25:52.826] Training 5 configs with budget of 4 for each 
    ## INFO  [16:25:52.830] Evaluating 5 configuration(s) 
    ## INFO  [16:25:53.212] Result of batch 7: 
    ## INFO  [16:25:53.214]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:53.214]  0.8397678  gbtree       4       3             1             4           4 
    ## INFO  [16:25:53.214]  0.3124482  gbtree       4       3             1             4           4 
    ## INFO  [16:25:53.214]  0.7082903    dart       4       3             1             4           4 
    ## INFO  [16:25:53.214]  0.5943432    dart       4       3             1             4           4 
    ## INFO  [16:25:53.214]  0.9131882  gbtree       4       3             1             4           4 
    ## INFO  [16:25:53.214]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:53.214]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:53.214]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:53.214]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:53.214]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:53.214]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:53.215] Training 2 configs with budget of 8 for each 
    ## INFO  [16:25:53.219] Evaluating 2 configuration(s) 
    ## INFO  [16:25:53.483] Result of batch 8: 
    ## INFO  [16:25:53.487]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:53.487]  0.8397678  gbtree       8       3             2             8           8 
    ## INFO  [16:25:53.487]  0.3124482  gbtree       8       3             2             8           8 
    ## INFO  [16:25:53.487]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:53.487]          2       0.04 <ResampleResult> 
    ## INFO  [16:25:53.487]          2       0.04 <ResampleResult> 
    ## INFO  [16:25:53.488] Training 1 configs with budget of 16 for each 
    ## INFO  [16:25:53.491] Evaluating 1 configuration(s) 
    ## INFO  [16:25:53.585] Result of batch 9: 
    ## INFO  [16:25:53.587]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:53.587]  0.8397678  gbtree      16       3             3            16          16 
    ## INFO  [16:25:53.587]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:53.587]          1       0.04 <ResampleResult> 
    ## INFO  [16:25:53.588] Start evaluation of bracket 3 
    ## INFO  [16:25:53.592] Training 7 configs with budget of 4 for each 
    ## INFO  [16:25:53.594] Evaluating 7 configuration(s) 
    ## INFO  [16:25:54.117] Result of batch 10: 
    ## INFO  [16:25:54.120]        eta  booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:54.120]  0.2387260   gbtree       4       2             0             4           4 
    ## INFO  [16:25:54.120]  0.9623589   gbtree       4       2             0             4           4 
    ## INFO  [16:25:54.120]  0.6013657   gbtree       4       2             0             4           4 
    ## INFO  [16:25:54.120]  0.5150297 gblinear       4       2             0             4           4 
    ## INFO  [16:25:54.120]  0.4025733   gbtree       4       2             0             4           4 
    ## INFO  [16:25:54.120]  0.8802465   gbtree       4       2             0             4           4 
    ## INFO  [16:25:54.120]  0.3640919     dart       4       2             0             4           4 
    ## INFO  [16:25:54.120]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:54.120]          7       0.04 <ResampleResult> 
    ## INFO  [16:25:54.120]          7       0.04 <ResampleResult> 
    ## INFO  [16:25:54.120]          7       0.04 <ResampleResult> 
    ## INFO  [16:25:54.120]          7       0.42 <ResampleResult> 
    ## INFO  [16:25:54.120]          7       0.04 <ResampleResult> 
    ## INFO  [16:25:54.120]          7       0.04 <ResampleResult> 
    ## INFO  [16:25:54.120]          7       0.04 <ResampleResult> 
    ## INFO  [16:25:54.121] Training 3 configs with budget of 8 for each 
    ## INFO  [16:25:54.124] Evaluating 3 configuration(s) 
    ## INFO  [16:25:54.378] Result of batch 11: 
    ## INFO  [16:25:54.380]        eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:54.380]  0.2387260  gbtree       8       2             1             8           8 
    ## INFO  [16:25:54.380]  0.9623589  gbtree       8       2             1             8           8 
    ## INFO  [16:25:54.380]  0.6013657  gbtree       8       2             1             8           8 
    ## INFO  [16:25:54.380]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:54.380]          3       0.04 <ResampleResult> 
    ## INFO  [16:25:54.380]          3       0.04 <ResampleResult> 
    ## INFO  [16:25:54.380]          3       0.04 <ResampleResult> 
    ## INFO  [16:25:54.381] Training 1 configs with budget of 16 for each 
    ## INFO  [16:25:54.384] Evaluating 1 configuration(s) 
    ## INFO  [16:25:54.481] Result of batch 12: 
    ## INFO  [16:25:54.484]       eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:54.484]  0.238726  gbtree      16       2             2            16          16 
    ## INFO  [16:25:54.484]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:54.484]          1       0.04 <ResampleResult> 
    ## INFO  [16:25:54.485] Start evaluation of bracket 4 
    ## INFO  [16:25:54.489] Training 5 configs with budget of 8 for each 
    ## INFO  [16:25:54.491] Evaluating 5 configuration(s) 
    ## INFO  [16:25:54.881] Result of batch 13: 
    ## INFO  [16:25:54.884]         eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:54.884]  0.04766363    dart       8       1             0             8           8 
    ## INFO  [16:25:54.884]  0.70085309  gbtree       8       1             0             8           8 
    ## INFO  [16:25:54.884]  0.35188864    dart       8       1             0             8           8 
    ## INFO  [16:25:54.884]  0.40894400    dart       8       1             0             8           8 
    ## INFO  [16:25:54.884]  0.82095132    dart       8       1             0             8           8 
    ## INFO  [16:25:54.884]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:54.884]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:54.884]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:54.884]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:54.884]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:54.884]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:54.885] Training 2 configs with budget of 16 for each 
    ## INFO  [16:25:54.888] Evaluating 2 configuration(s) 
    ## INFO  [16:25:55.069] Result of batch 14: 
    ## INFO  [16:25:55.072]         eta booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:55.072]  0.04766363    dart      16       1             1            16          16 
    ## INFO  [16:25:55.072]  0.70085309  gbtree      16       1             1            16          16 
    ## INFO  [16:25:55.072]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:55.072]          2       0.04 <ResampleResult> 
    ## INFO  [16:25:55.072]          2       0.04 <ResampleResult> 
    ## INFO  [16:25:55.073] Start evaluation of bracket 5 
    ## INFO  [16:25:55.076] Training 5 configs with budget of 16 for each 
    ## INFO  [16:25:55.079] Evaluating 5 configuration(s) 
    ## INFO  [16:25:55.479] Result of batch 15: 
    ## INFO  [16:25:55.482]         eta  booster nrounds bracket bracket_stage budget_scaled budget_real 
    ## INFO  [16:25:55.482]  0.05284394     dart      16       0             0            16          16 
    ## INFO  [16:25:55.482]  0.39522013 gblinear      16       0             0            16          16 
    ## INFO  [16:25:55.482]  0.47784538 gblinear      16       0             0            16          16 
    ## INFO  [16:25:55.482]  0.56025326 gblinear      16       0             0            16          16 
    ## INFO  [16:25:55.482]  0.69826159   gbtree      16       0             0            16          16 
    ## INFO  [16:25:55.482]  n_configs classif.ce  resample_result 
    ## INFO  [16:25:55.482]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:55.482]          5       0.30 <ResampleResult> 
    ## INFO  [16:25:55.482]          5       0.28 <ResampleResult> 
    ## INFO  [16:25:55.482]          5       0.20 <ResampleResult> 
    ## INFO  [16:25:55.482]          5       0.04 <ResampleResult> 
    ## INFO  [16:25:55.483] Done. 
    ## INFO  [16:25:55.491] Finished optimizing after 72 evaluation(s) 
    ## INFO  [16:25:55.492] Result: 
    ## INFO  [16:25:55.494]  nrounds       eta booster learner_param_vals x_domain classif.ce 
    ## INFO  [16:25:55.494]    <num>     <num>  <char>             <list>   <list>      <num> 
    ## INFO  [16:25:55.494]        1 0.8474532    dart             <list>   <list>       0.04

``` r
# return best result
inst$result
```

    ##    nrounds       eta booster learner_param_vals x_domain classif.ce
    ##      <num>     <num>  <char>             <list>   <list>      <num>
    ## 1:       1 0.8474532    dart             <list>   <list>       0.04

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

# define extended hyperparameters with subsampling fraction as budget
# ==> no learner budget is required
params = list(
  ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
  ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
  ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
)

# define TuningInstance with the Graph Learner and the extended hyperparams
inst = TuningInstanceSingleCrit$new(
  tsk("iris"),
  ll,
  rsmp("holdout"),
  msr("classif.ce"),
  ParamSet$new(params),
  trm("none") # hyperband terminates on its own
)

tuner = tnr("hyperband", eta = 4L)
tuner$optimize(inst)
```

    ## INFO  [16:25:55.934] Starting to optimize 3 parameter(s) with '<TunerHyperband>' and '<TerminatorNone>' 
    ## INFO  [16:25:55.948] Amount of brackets to be evaluated = 2,  
    ## INFO  [16:25:55.949] Start evaluation of bracket 1 
    ## INFO  [16:25:55.955] Training 4 configs with budget of 0.25 for each 
    ## INFO  [16:25:55.958] Evaluating 4 configuration(s) 
    ## INFO  [16:25:56.866] Result of batch 1: 
    ## INFO  [16:25:56.869]  classif.rpart.cp classif.rpart.minsplit subsample.frac bracket bracket_stage 
    ## INFO  [16:25:56.869]        0.02532664                      4           0.25       1             0 
    ## INFO  [16:25:56.869]        0.07348139                      3           0.25       1             0 
    ## INFO  [16:25:56.869]        0.08489786                      2           0.25       1             0 
    ## INFO  [16:25:56.869]        0.05025520                      4           0.25       1             0 
    ## INFO  [16:25:56.869]  budget_scaled budget_real n_configs classif.ce  resample_result 
    ## INFO  [16:25:56.869]            2.5        0.25         4       0.04 <ResampleResult> 
    ## INFO  [16:25:56.869]            2.5        0.25         4       0.02 <ResampleResult> 
    ## INFO  [16:25:56.869]            2.5        0.25         4       0.08 <ResampleResult> 
    ## INFO  [16:25:56.869]            2.5        0.25         4       0.02 <ResampleResult> 
    ## INFO  [16:25:56.870] Training 1 configs with budget of 1 for each 
    ## INFO  [16:25:56.873] Evaluating 1 configuration(s) 
    ## INFO  [16:25:57.168] Result of batch 2: 
    ## INFO  [16:25:57.171]  classif.rpart.cp classif.rpart.minsplit subsample.frac bracket bracket_stage 
    ## INFO  [16:25:57.171]        0.07348139                      3              1       1             1 
    ## INFO  [16:25:57.171]  budget_scaled budget_real n_configs classif.ce  resample_result 
    ## INFO  [16:25:57.171]             10           1         1       0.04 <ResampleResult> 
    ## INFO  [16:25:57.172] Start evaluation of bracket 2 
    ## INFO  [16:25:57.176] Training 2 configs with budget of 1 for each 
    ## INFO  [16:25:57.178] Evaluating 2 configuration(s) 
    ## INFO  [16:25:57.624] Result of batch 3: 
    ## INFO  [16:25:57.627]  classif.rpart.cp classif.rpart.minsplit subsample.frac bracket bracket_stage 
    ## INFO  [16:25:57.627]        0.05762160                      5              1       0             0 
    ## INFO  [16:25:57.627]        0.02247238                      3              1       0             0 
    ## INFO  [16:25:57.627]  budget_scaled budget_real n_configs classif.ce  resample_result 
    ## INFO  [16:25:57.627]             10           1         2       0.04 <ResampleResult> 
    ## INFO  [16:25:57.627]             10           1         2       0.04 <ResampleResult> 
    ## INFO  [16:25:57.628] Done. 
    ## INFO  [16:25:57.644] Finished optimizing after 7 evaluation(s) 
    ## INFO  [16:25:57.645] Result: 
    ## INFO  [16:25:57.647]  classif.rpart.cp classif.rpart.minsplit subsample.frac learner_param_vals 
    ## INFO  [16:25:57.647]             <num>                  <int>          <num>             <list> 
    ## INFO  [16:25:57.647]        0.07348139                      3           0.25             <list> 
    ## INFO  [16:25:57.647]  x_domain classif.ce 
    ## INFO  [16:25:57.647]    <list>      <num> 
    ## INFO  [16:25:57.647]    <list>       0.02

``` r
# return best result
inst$result
```

    ##    classif.rpart.cp classif.rpart.minsplit subsample.frac learner_param_vals
    ##               <num>                  <int>          <num>             <list>
    ## 1:       0.07348139                      3           0.25             <list>
    ##    x_domain classif.ce
    ##      <list>      <num>
    ## 1:   <list>       0.02

## Documentation

The function reference is can be found
[here](https://mlr3hyperband.mlr-org.com/reference/). Further
documentation lives in the [mlr3book](https://mlr3book.mlr-org.com/).

The original paper introducing the hyperband algorithm is given
[here](https://arxiv.org/abs/1603.06560).
