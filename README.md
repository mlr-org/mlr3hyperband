
# mlr3hyperband <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3hyperband.mlr-org.com/) |
[dev](https://mlr3hyperband.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3hyperband/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3hyperband/actions/workflows/r-cmd-check.yml)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3hyperband)](https://cran.r-project.org/package=mlr3hyperband)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

*mlr3hyperband* adds the optimization algorithms Successive Halving
(Jamieson and Talwalkar 2016) and Hyperband (Li et al. 2018) to the
[mlr3](https://mlr-org.com/) ecosystem. The implementation in
mlr3hyperband features improved scheduling and parallelizes the
evaluation of configurations. The package includes tuners for
hyperparameter optimization in
[mlr3tuning](https://github.com/mlr-org/mlr3tuning) and optimizers for
black-box optimization in [bbotk](https://github.com/mlr-org/bbotk).

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

The [gallery](https://mlr-org.com/gallery-all-optimization.html)
features a series of case studies on Hyperband.

  - [Tune](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/)
    the hyperparameters of XGBoost with Hyperband
  - Use data
    [subsampling](https://mlr-org.com/gallery/series/2023-01-16-hyperband-subsampling/)
    and Hyperband to optimize a support vector machine.

## Examples

We optimize the hyperparameters of an XGBoost model on the
[Sonar](https://mlr3.mlr-org.com/reference/mlr_tasks_sonar.html) data
set. The number of boosting rounds `nrounds` is the fidelity parameter.
We tag this parameter with `"budget"` in the search space.

``` r
library(mlr3hyperband)
library(mlr3learners)

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
```

We use the `tune()` function to run the optimization.

``` r
instance = tune(
  method = "hyperband",
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  eta = 3
)
```

The instance contains the best-performing hyperparameter configuration.

``` r
instance$result
```

    ##    nrounds       eta max_depth colsample_bytree colsample_bylevel    lambda     alpha subsample
    ## 1:      27 -2.102951         3        0.7175178         0.5419011 -5.390012 -4.696385  0.193622
    ## 3 variables not shown: [learner_param_vals, x_domain, classif.ce]

The archive contains all evaluated hyperparameter configurations.
Hyperband adds the `"stage"` and `"braket"`.

``` r
as.data.table(instance$archive)[, .(stage, bracket, classif.ce, nrounds)]
```

    ##     stage bracket classif.ce nrounds
    ##  1:     0       2  0.3489583      27
    ##  2:     0       2  0.2434896      27
    ##  3:     0       2  0.2591146      27
    ##  4:     0       2  0.3489583      27
    ##  5:     0       2  0.5052083      27
    ## ---                                 
    ## 18:     0       0  0.2434896     243
    ## 19:     0       0  0.4960938     243
    ## 20:     0       0  0.2903646     243
    ## 21:     2       2  0.2473958     243
    ## 22:     1       1  0.2421875     243

We fit a final model with optimized hyperparameters to make predictions
on new data.

``` r
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("sonar"))
```

## References

<div id="refs" class="references hanging-indent">

<div id="ref-jamieson_2016">

Jamieson, Kevin, and Ameet Talwalkar. 2016. “Non-Stochastic Best Arm
Identification and Hyperparameter Optimization.” In *Proceedings of the
19th International Conference on Artificial Intelligence and
Statistics*, edited by Arthur Gretton and Christian C. Robert,
51:240–48. Proceedings of Machine Learning Research. Cadiz, Spain:
PMLR. <http://proceedings.mlr.press/v51/jamieson16.html>.

</div>

<div id="ref-li_2018">

Li, Lisha, Kevin Jamieson, Giulia DeSalvo, Afshin Rostamizadeh, and
Ameet Talwalkar. 2018. “Hyperband: A Novel Bandit-Based Approach to
Hyperparameter Optimization.” *Journal of Machine Learning Research* 18
(185): 1–52. <https://jmlr.org/papers/v18/16-558.html>.

</div>

</div>
