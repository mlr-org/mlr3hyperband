# Hyperparameter Optimization with Successive Halving

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

## Resources

The [gallery](https://mlr-org.com/gallery-all-optimization.html)
features a collection of case studies and demos about optimization.

- [Tune](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/)
  the hyperparameters of XGBoost with Hyperband (Hyperband can be easily
  swapped with SHA).

- Use data
  [subsampling](https://mlr-org.com/gallery/series/2023-01-16-hyperband-subsampling/)
  and Hyperband to optimize a support vector machine.

## Dictionary

This
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[bbotk::mlr_optimizers](https://bbotk.mlr-org.com/reference/mlr_optimizers.html)
or with the associated sugar function
[`bbotk::opt()`](https://bbotk.mlr-org.com/reference/opt.html):

    mlr_optimizers$get("successive_halving")
    opt("successive_halving")

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

## Logging

Hyperband uses a logger (as implemented in
[lgr](https://CRAN.R-project.org/package=lgr)) from package
[bbotk](https://CRAN.R-project.org/package=bbotk). Use
`lgr::get_logger("bbotk")` to access and control the logger.

## Super classes

[`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
-\>
[`bbotk::OptimizerBatch`](https://bbotk.mlr-org.com/reference/OptimizerBatch.html)
-\> `OptimizerBatchSuccessiveHalving`

## Methods

### Public methods

- [`OptimizerBatchSuccessiveHalving$new()`](#method-OptimizerBatchSuccessiveHalving-new)

- [`OptimizerBatchSuccessiveHalving$clone()`](#method-OptimizerBatchSuccessiveHalving-clone)

Inherited methods

- [`bbotk::Optimizer$format()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-format)
- [`bbotk::Optimizer$help()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-help)
- [`bbotk::Optimizer$print()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-print)
- [`bbotk::OptimizerBatch$optimize()`](https://bbotk.mlr-org.com/reference/OptimizerBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OptimizerBatchSuccessiveHalving$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerBatchSuccessiveHalving$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(bbotk)
library(data.table)

# set search space
search_space = domain = ps(
  x1 = p_dbl(-5, 10),
  x2 = p_dbl(0, 15),
  fidelity = p_dbl(1e-2, 1, tags = "budget")
)

# Branin function with fidelity, see `bbotk::branin()`
fun = function(xs) branin_wu(xs[["x1"]], xs[["x2"]], xs[["fidelity"]])

# create objective
objective = ObjectiveRFun$new(
  fun = fun,
  domain = domain,
  codomain = ps(y = p_dbl(tags = "minimize"))
)

# initialize instance and optimizer
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  search_space = search_space,
  terminator = trm("evals", n_evals = 50)
)
#> OptimInstanceSingleCrit is deprecated. Use OptimInstanceBatchSingleCrit instead.

optimizer = opt("successive_halving")

# optimize branin function
optimizer$optimize(instance)
#>           x1       x2 fidelity  x_domain        y
#>        <num>    <num>    <num>    <list>    <num>
#> 1: -1.845339 9.839739     0.16 <list[3]> 7.957692

# best scoring evaluation
instance$result
#>           x1       x2 fidelity  x_domain        y
#>        <num>    <num>    <num>    <list>    <num>
#> 1: -1.845339 9.839739     0.16 <list[3]> 7.957692

# all evaluations
as.data.table(instance$archive)
#>             x1         x2 fidelity stage repetition          y
#>          <num>      <num>    <num> <int>      <num>      <num>
#>  1: -2.8202623  6.2590489     0.01     0          1  20.866882
#>  2:  7.8157583  2.0711226     0.01     0          1  54.808291
#>  3: -1.8027603  1.2126744     0.01     0          1  67.926487
#>  4: -1.8453389  9.8397394     0.01     0          1   8.036810
#>  5: -4.4071897  9.0300579     0.01     0          1  28.004065
#>  6:  9.1716220  9.8549374     0.01     0          1 253.925823
#>  7: -1.3260802  4.9397575     0.01     0          1  22.719578
#>  8:  6.7168385 14.6921133     0.01     0          1 343.451440
#>  9: -0.6764425 10.7277920     0.01     0          1  30.718370
#> 10:  8.1303687 13.0894546     0.01     0          1 332.606891
#> 11: -0.5637486 14.7492562     0.01     0          1  79.619962
#> 12:  9.7528811  3.2784449     0.01     0          1  99.505602
#> 13:  3.8475633  9.9679510     0.01     0          1  95.713095
#> 14:  6.3873757  5.8434606     0.01     0          1  96.599814
#> 15:  7.5411296  0.6909546     0.01     0          1  37.720661
#> 16:  6.4422920  9.2537184     0.01     0          1 169.646079
#> 17: -1.8453389  9.8397394     0.02     1          1   8.031374
#> 18: -2.8202623  6.2590489     0.02     1          1  20.938047
#> 19: -1.3260802  4.9397575     0.02     1          1  22.730919
#> 20: -4.4071897  9.0300579     0.02     1          1  28.181990
#> 21: -0.6764425 10.7277920     0.02     1          1  30.715041
#> 22:  7.5411296  0.6909546     0.02     1          1  37.157884
#> 23:  7.8157583  2.0711226     0.02     1          1  53.997570
#> 24: -1.8027603  1.2126744     0.02     1          1  67.976901
#> 25: -1.8453389  9.8397394     0.04     2          1   8.020569
#> 26: -2.8202623  6.2590489     0.04     2          1  21.080756
#> 27: -1.3260802  4.9397575     0.04     2          1  22.753620
#> 28: -4.4071897  9.0300579     0.04     2          1  28.540104
#> 29: -1.8453389  9.8397394     0.08     3          1   7.999239
#> 30: -2.8202623  6.2590489     0.08     3          1  21.367693
#> 31: -1.8453389  9.8397394     0.16     4          1   7.957692
#>             x1         x2 fidelity stage repetition          y
#>          <num>      <num>    <num> <int>      <num>      <num>
#>               timestamp batch_nr x_domain_x1 x_domain_x2 x_domain_fidelity
#>                  <POSc>    <int>       <num>       <num>             <num>
#>  1: 2026-03-16 13:06:36        1  -2.8202623   6.2590489              0.01
#>  2: 2026-03-16 13:06:36        1   7.8157583   2.0711226              0.01
#>  3: 2026-03-16 13:06:36        1  -1.8027603   1.2126744              0.01
#>  4: 2026-03-16 13:06:36        1  -1.8453389   9.8397394              0.01
#>  5: 2026-03-16 13:06:36        1  -4.4071897   9.0300579              0.01
#>  6: 2026-03-16 13:06:36        1   9.1716220   9.8549374              0.01
#>  7: 2026-03-16 13:06:36        1  -1.3260802   4.9397575              0.01
#>  8: 2026-03-16 13:06:36        1   6.7168385  14.6921133              0.01
#>  9: 2026-03-16 13:06:36        1  -0.6764425  10.7277920              0.01
#> 10: 2026-03-16 13:06:36        1   8.1303687  13.0894546              0.01
#> 11: 2026-03-16 13:06:36        1  -0.5637486  14.7492562              0.01
#> 12: 2026-03-16 13:06:36        1   9.7528811   3.2784449              0.01
#> 13: 2026-03-16 13:06:36        1   3.8475633   9.9679510              0.01
#> 14: 2026-03-16 13:06:36        1   6.3873757   5.8434606              0.01
#> 15: 2026-03-16 13:06:36        1   7.5411296   0.6909546              0.01
#> 16: 2026-03-16 13:06:36        1   6.4422920   9.2537184              0.01
#> 17: 2026-03-16 13:06:36        2  -1.8453389   9.8397394              0.02
#> 18: 2026-03-16 13:06:36        2  -2.8202623   6.2590489              0.02
#> 19: 2026-03-16 13:06:36        2  -1.3260802   4.9397575              0.02
#> 20: 2026-03-16 13:06:36        2  -4.4071897   9.0300579              0.02
#> 21: 2026-03-16 13:06:36        2  -0.6764425  10.7277920              0.02
#> 22: 2026-03-16 13:06:36        2   7.5411296   0.6909546              0.02
#> 23: 2026-03-16 13:06:36        2   7.8157583   2.0711226              0.02
#> 24: 2026-03-16 13:06:36        2  -1.8027603   1.2126744              0.02
#> 25: 2026-03-16 13:06:37        3  -1.8453389   9.8397394              0.04
#> 26: 2026-03-16 13:06:37        3  -2.8202623   6.2590489              0.04
#> 27: 2026-03-16 13:06:37        3  -1.3260802   4.9397575              0.04
#> 28: 2026-03-16 13:06:37        3  -4.4071897   9.0300579              0.04
#> 29: 2026-03-16 13:06:37        4  -1.8453389   9.8397394              0.08
#> 30: 2026-03-16 13:06:37        4  -2.8202623   6.2590489              0.08
#> 31: 2026-03-16 13:06:37        5  -1.8453389   9.8397394              0.16
#>               timestamp batch_nr x_domain_x1 x_domain_x2 x_domain_fidelity
#>                  <POSc>    <int>       <num>       <num>             <num>
```
