# Tuner Using the Hyperband Algorithm

Optimizer using the Hyperband (HB) algorithm. HB runs the [Successive
Halving
Algorithm](https://mlr3hyperband.mlr-org.com/reference/mlr_optimizers_successive_halving.md)
(SHA) with different numbers of stating configurations. The algorithm is
initialized with the same parameters as Successive Halving but without
`n`. Each run of Successive Halving is called a bracket and starts with
a different budget `r_0`. A smaller starting budget means that more
configurations can be tried out. The most explorative bracket allocated
the minimum budget `r_min`. The next bracket increases the starting
budget by a factor of `eta`. In each bracket, the starting budget
increases further until the last bracket `s = 0` essentially performs a
random search with the full budget `r_max`. The number of brackets
`s_max + 1` is calculated with `s_max = log(r_min / r_max)(eta)`. Under
the condition that `r_0` increases by `eta` with each bracket, `r_min`
sometimes has to be adjusted slightly in order not to use more than
`r_max` resources in the last bracket. The number of configurations in
the base stages is calculated so that each bracket uses approximately
the same amount of budget. The following table shows a full run of HB
with `eta = 2`, `r_min = 1` and `r_max = 8`.

|     |     |       |       |     |       |       |     |       |       |     |       |       |
|-----|-----|-------|-------|-----|-------|-------|-----|-------|-------|-----|-------|-------|
| `s` |     |       | 3     |     |       | 2     |     |       | 1     |     |       | 0     |
| `i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |
| 0   |     | 8     | 1     |     | 6     | 2     |     | 4     | 4     |     | 8     | 4     |
| 1   |     | 4     | 2     |     | 3     | 4     |     | 2     | 8     |     |       |       |
| 2   |     | 2     | 4     |     | 1     | 8     |     |       |       |     |       |       |
| 3   |     | 1     | 8     |     |       |       |     |       |       |     |       |       |

`s` is the bracket number, `i` is the stage number, `n_i` is the number
of configurations and `r_i` is the budget allocated to a single
configuration.

The budget hyperparameter must be tagged with `"budget"` in the search
space. The minimum budget (`r_min`) which is allocated in the base stage
of the most explorative bracket, is set by the lower bound of the budget
parameter. The upper bound defines the maximum budget (`r_max`) which is
allocated to the candidates in the last stages.

## Source

Li L, Jamieson K, DeSalvo G, Rostamizadeh A, Talwalkar A (2018).
“Hyperband: A Novel Bandit-Based Approach to Hyperparameter
Optimization.” *Journal of Machine Learning Research*, **18**(185),
1-52. <https://jmlr.org/papers/v18/16-558.html>.

## Dictionary

This
[mlr3tuning::Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3tuning::mlr_tuners](https://mlr3tuning.mlr-org.com/reference/mlr_tuners.html)
or with the associated sugar function
[`mlr3tuning::tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.html):

    TunerBatchHyperband$new()
    mlr_tuners$get("hyperband")
    tnr("hyperband")

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

This hyperband implementation evaluates hyperparameter configurations of
equal budget across brackets in one batch. For example, all
configurations in stage 1 of bracket 3 and stage 0 of bracket 2 in one
batch. To select a parallel backend, use the `plan()` function of the
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
  the hyperparameters of XGBoost with Hyperband.

- Use data
  [subsampling](https://mlr-org.com/gallery/series/2023-01-16-hyperband-subsampling/)
  and Hyperband to optimize a support vector machine.

## Parameters

- `eta`:

  `numeric(1)`  
  With every stage, the budget is increased by a factor of `eta` and
  only the best `1 / eta` points are promoted to the next stage.
  Non-integer values are supported, but `eta` is not allowed to be less
  or equal to 1.

- `sampler`:

  [paradox::Sampler](https://paradox.mlr-org.com/reference/Sampler.html)  
  Object defining how the samples of the parameter space should be drawn
  in the base stage of each bracket. The default is uniform sampling.

- `repetitions`:

  `integer(1)`  
  If `1` (default), optimization is stopped once all brackets are
  evaluated. Otherwise, optimization is stopped after `repetitions` runs
  of HB. The
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  might stop the optimization before all repetitions are executed.

## Archive

The [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html)
holds the following additional columns that are specific to HB:

- `bracket` (`integer(1)`)  
  The bracket index. Counts down to 0.

- `stage` (`integer(1))`  
  The stages of each bracket. Starts counting at 0.

- `repetition` (`integer(1))`  
  Repetition index. Start counting at 1.

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatch.html)
-\>
[`mlr3tuning::TunerBatchFromOptimizerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.html)
-\> `TunerBatchHyperband`

## Methods

### Public methods

- [`TunerBatchHyperband$new()`](#method-TunerBatchHyperband-new)

- [`TunerBatchHyperband$clone()`](#method-TunerBatchHyperband-clone)

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

    TunerBatchHyperband$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchHyperband$clone(deep = FALSE)

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
    tnr("hyperband"),
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
#> Loading required namespace: xgboost
#>    nrounds       eta booster learner_param_vals  x_domain classif.ce
#>      <num>     <num>  <char>             <list>    <list>      <num>
#> 1:       8 0.3758337  gbtree          <list[7]> <list[3]>  0.2317708
```
