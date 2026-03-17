# Hyperparameter Tuning with Successive Halving

Optimizer using the Successive Halving Algorithm (SHA). SHA is
initialized with the number of starting configurations `n`, the
proportion of configurations discarded in each stage `eta`, and the
minimum `r_min` and maximum `_max` budget of a single evaluation. The
algorithm starts by sampling `n` random configurations and allocating
the minimum budget `r_min` to them. The configurations are evaluated and
`1 / eta` of the worst-performing configurations are discarded. The
remaining configurations are promoted to the next stage and evaluated on
a larger budget. The following table is the stage layout for `eta = 2`,
`r_min = 1` and `r_max = 8`.

|     |       |       |
|-----|-------|-------|
| `i` | `n_i` | `r_i` |
| 0   | 8     | 1     |
| 1   | 4     | 2     |
| 2   | 2     | 4     |
| 3   | 1     | 8     |

`i` is the stage number, `n_i` is the number of configurations and `r_i`
is the budget allocated to a single configuration.

The number of stages is calculated so that each stage consumes
approximately the same budget. This sometimes results in the minimum
budget having to be slightly adjusted by the algorithm.

## Source

Jamieson K, Talwalkar A (2016). “Non-stochastic Best Arm Identification
and Hyperparameter Optimization.” In Gretton A, Robert CC (eds.),
*Proceedings of the 19th International Conference on Artificial
Intelligence and Statistics*, volume 51 series Proceedings of Machine
Learning Research, 240-248.
[http://proceedings.mlr.press/v51/jamieson16.html](http://proceedings.mlr.press/v51/jamieson16.md).

## Dictionary

This
[mlr3tuning::Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3tuning::mlr_tuners](https://mlr3tuning.mlr-org.com/reference/mlr_tuners.html)
or with the associated sugar function
[`mlr3tuning::tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.html):

    TunerBatchSuccessiveHalving$new()
    mlr_tuners$get("successive_halving")
    tnr("successive_halving")

## Subsample Budget

If the learner lacks a natural budget parameter,
[mlr3pipelines::PipeOpSubsample](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.html)
can be applied to use the subsampling rate as budget parameter. The
resulting
[mlr3pipelines::GraphLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_graph.html)
is fitted on small proportions of the
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) in the first
stage, and on the complete task in last stage.

## Custom Sampler

Hyperband supports custom
[paradox::Sampler](https://paradox.mlr-org.com/reference/Sampler.html)
object for initial configurations in each bracket. A custom sampler may
look like this (the full example is given in the *examples* section):

    # - beta distribution with alpha = 2 and beta = 5
    # - categorical distribution with custom probabilities
    sampler = SamplerJointIndep$new(list(
      Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
      Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
    ))

## Progress Bars

`$optimize()` supports progress bars via the package
[progressr](https://CRAN.R-project.org/package=progressr) combined with
a
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).
Simply wrap the function in `progressr::with_progress()` to enable them.
We recommend to use package
[progress](https://CRAN.R-project.org/package=progress) as backend;
enable with `progressr::handlers("progress")`.

## Parallelization

The hyperparameter configurations of one stage are evaluated in parallel
with the [future](https://CRAN.R-project.org/package=future) package. To
select a parallel backend, use the `plan()` function of the
[future](https://CRAN.R-project.org/package=future) package.

## Logging

Hyperband uses a logger (as implemented in
[lgr](https://CRAN.R-project.org/package=lgr)) from package
[bbotk](https://CRAN.R-project.org/package=bbotk). Use
`lgr::get_logger("bbotk")` to access and control the logger.

## Resources

The [gallery](https://mlr-org.com/gallery-all-optimization.html)
features a collection of case studies and demos about optimization.

- [Tune](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/)
  the hyperparameters of XGBoost with Hyperband (Hyperband can be easily
  swapped with SHA).

- Use data
  [subsampling](https://mlr-org.com/gallery/series/2023-01-16-hyperband-subsampling/)
  and Hyperband to optimize a support vector machine.

## Parameters

- `n`:

  `integer(1)`  
  Number of configurations in the base stage.

- `eta`:

  `numeric(1)`  
  With every stage, the budget is increased by a factor of `eta` and
  only the best `1 / eta` configurations are promoted to the next stage.
  Non-integer values are supported, but `eta` is not allowed to be less
  or equal to 1.

- `sampler`:

  [paradox::Sampler](https://paradox.mlr-org.com/reference/Sampler.html)  
  Object defining how the samples of the parameter space should be
  drawn. The default is uniform sampling.

- `repetitions`:

  `integer(1)`  
  If `1` (default), optimization is stopped once all stages are
  evaluated. Otherwise, optimization is stopped after `repetitions` runs
  of SHA. The
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  might stop the optimization before all repetitions are executed.

- `adjust_minimum_budget`:

  `logical(1)`  
  If `TRUE`, the minimum budget is increased so that the last stage uses
  the maximum budget defined in the search space.

## Archive

The [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html)
holds the following additional columns that are specific to SHA:

- `stage` (`integer(1))`  
  Stage index. Starts counting at 0.

- `repetition` (`integer(1))`  
  Repetition index. Start counting at 1.

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatch.html)
-\>
[`mlr3tuning::TunerBatchFromOptimizerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.html)
-\> `TunerBatchSuccessiveHalving`

## Methods

### Public methods

- [`TunerBatchSuccessiveHalving$new()`](#method-TunerBatchSuccessiveHalving-new)

- [`TunerBatchSuccessiveHalving$clone()`](#method-TunerBatchSuccessiveHalving-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)
- [`mlr3tuning::TunerBatchFromOptimizerBatch$optimize()`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerBatchSuccessiveHalving$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchSuccessiveHalving$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if(requireNamespace("xgboost")) {
  library(mlr3learners)

  # define hyperparameter and budget parameter
  search_space = ps(
    nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
    eta = p_dbl(lower = 0, upper = 1),
    booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
  )

  # \donttest{
  # hyperparameter tuning on the pima indians diabetes data set
  instance = tune(
    tnr("successive_halving"),
    task = tsk("pima"),
    learner = lrn("classif.xgboost", eval_metric = "logloss"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    search_space = search_space,
    term_evals = 100
  )

  # best performing hyperparameter configuration
  instance$result
  # }
}
#>    nrounds       eta booster learner_param_vals  x_domain classif.ce
#>      <num>     <num>  <char>             <list>    <list>      <num>
#> 1:       4 0.3454555  gbtree          <list[7]> <list[3]>  0.2408854
```
