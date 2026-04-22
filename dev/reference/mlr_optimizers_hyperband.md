# Optimizer Using the Hyperband Algorithm

Optimizer using the Hyperband (HB) algorithm. HB runs the [Successive
Halving
Algorithm](https://mlr3hyperband.mlr-org.com/dev/reference/mlr_optimizers_successive_halving.md)
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

## Resources

The [gallery](https://mlr-org.com/gallery-all-optimization.html)
features a collection of case studies and demos about optimization.

- [Tune](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/)
  the hyperparameters of XGBoost with Hyperband.

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

    mlr_optimizers$get("hyperband")
    opt("hyperband")

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
-\> `OptimizerBatchHyperband`

## Methods

### Public methods

- [`OptimizerBatchHyperband$new()`](#method-OptimizerBatchHyperband-new)

- [`OptimizerBatchHyperband$clone()`](#method-OptimizerBatchHyperband-clone)

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

    OptimizerBatchHyperband$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerBatchHyperband$clone(deep = FALSE)

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

optimizer = opt("hyperband")

# optimize branin function
optimizer$optimize(instance)
#>          x1       x2 fidelity  x_domain        y
#>       <num>    <num>    <num>    <list>    <num>
#> 1: 2.953187 2.235532 0.015625 <list[3]> 1.013389

# best scoring evaluation
instance$result
#>          x1       x2 fidelity  x_domain        y
#>       <num>    <num>    <num>    <list>    <num>
#> 1: 2.953187 2.235532 0.015625 <list[3]> 1.013389

# all evaluations
as.data.table(instance$archive)
#>              x1         x2 fidelity stage bracket repetition          y
#>           <num>      <num>    <num> <num>   <int>      <num>      <num>
#>  1: -3.78874794  8.9439418 0.015625     0       6          1  14.781959
#>  2:  7.51499556  2.8727705 0.015625     0       6          1  63.557786
#>  3:  4.01141329 14.2164591 0.015625     0       6          1 202.787861
#>  4: -2.64187338  8.1372061 0.015625     0       6          1   6.779880
#>  5: -4.88900838  8.1690509 0.015625     0       6          1  51.971208
#>  6:  1.99590246  4.1789573 0.015625     0       6          1   7.560334
#>  7:  2.46666083  6.7005370 0.015625     0       6          1  22.210292
#>  8: -0.65349133  5.5726678 0.015625     0       6          1  19.815710
#>  9:  5.99322981  0.4209146 0.015625     0       6          1  27.352543
#> 10:  6.58782267  6.9898079 0.015625     0       6          1 121.984687
#> 11:  8.11900991  5.8504708 0.015625     0       6          1 122.950307
#> 12: -2.37589060  0.3009783 0.015625     0       6          1  96.276269
#> 13: -4.48638001  5.6545639 0.015625     0       6          1  73.532730
#> 14: -0.19421404  8.3986926 0.015625     0       6          1  23.783136
#> 15:  1.03492358 12.8562538 0.015625     0       6          1  86.651345
#> 16: -2.06495248  5.7721457 0.015625     0       6          1  18.735016
#> 17:  1.05307176  7.9187555 0.015625     0       6          1  27.430526
#> 18: -4.04507814  9.0095629 0.015625     0       6          1  19.514032
#> 19:  0.83051970  3.9205704 0.015625     0       6          1  17.083118
#> 20:  9.63321753  4.3507524 0.015625     0       6          1 117.877434
#> 21: -0.65161557  7.2011276 0.015625     0       6          1  17.657487
#> 22:  5.17570641 13.8000832 0.015625     0       6          1 245.751887
#> 23:  6.02979398  6.0108028 0.015625     0       6          1  91.368896
#> 24: -2.06064900  3.1975907 0.015625     0       6          1  44.078574
#> 25:  9.70809512 10.0765022 0.015625     0       6          1 277.324244
#> 26:  6.12282294  0.8792117 0.015625     0       6          1  31.528931
#> 27: -4.22830586 14.9560370 0.015625     0       6          1   8.342752
#> 28:  2.95318695  2.2355320 0.015625     0       6          1   1.013389
#> 29:  5.43735818  7.7783495 0.015625     0       6          1 107.057527
#> 30:  5.32834005 12.6918008 0.015625     0       6          1 220.013816
#> 31: -4.53154512 10.7740459 0.015625     0       6          1  17.694913
#> 32: -1.61656198  3.6197103 0.015625     0       6          1  34.896610
#> 33: -0.48753791  8.2056505 0.015625     0       6          1  20.506586
#> 34:  4.54698422 12.5220272 0.015625     0       6          1 180.634251
#> 35:  2.18536825  0.4193404 0.015625     0       6          1   9.523050
#> 36:  1.48256887  7.0407645 0.015625     0       6          1  21.953386
#> 37:  5.59650757 12.0852005 0.015625     0       6          1 214.246974
#> 38:  9.22864864 12.2107697 0.015625     0       6          1 334.738923
#> 39: -2.29491848  6.0586650 0.015625     0       6          1  17.744430
#> 40: -1.74650185  3.2764651 0.015625     0       6          1  39.647547
#> 41:  5.20244376  6.2754210 0.015625     0       6          1  74.167080
#> 42:  2.48268416 10.0330612 0.015625     0       6          1  63.167824
#> 43:  4.62519023  7.6147542 0.015625     0       6          1  78.356573
#> 44:  4.90426524  9.9053896 0.015625     0       6          1 132.199431
#> 45: -3.55963763  7.6768697 0.015625     0       6          1  20.392237
#> 46:  6.48400246 12.5332866 0.015625     0       6          1 261.529668
#> 47:  6.54512206 10.6317174 0.015625     0       6          1 207.827119
#> 48:  9.86068468 13.1130891 0.015625     0       6          1 394.018374
#> 49:  9.55781354  0.1721931 0.015625     0       6          1  43.715083
#> 50:  0.83774141 13.3237435 0.015625     0       6          1  90.996559
#> 51:  1.91779697 14.9452038 0.015625     0       6          1 147.973171
#> 52: -0.27137371  7.5028725 0.015625     0       6          1  20.392837
#> 53: -2.37986159  5.3845054 0.015625     0       6          1  24.003227
#> 54:  2.97360311 11.6236953 0.015625     0       6          1 102.229329
#> 55:  2.40455524  8.7671288 0.015625     0       6          1  44.059174
#> 56:  6.68962939  9.5096456 0.015625     0       6          1 182.162506
#> 57: -1.93732486 12.8799923 0.015625     0       6          1  20.110380
#> 58:  5.70095918  8.5034151 0.015625     0       6          1 129.902880
#> 59: -4.02175832  3.7949553 0.015625     0       6          1  86.751308
#> 60:  0.31310198 13.7820482 0.015625     0       6          1  87.649828
#> 61:  7.37799132 13.0102531 0.015625     0       6          1 306.091599
#> 62: -0.89272632  3.7280805 0.015625     0       6          1  29.841207
#> 63:  3.55067427  6.0432182 0.015625     0       6          1  29.350783
#> 64:  0.03578621 11.5444526 0.015625     0       6          1  50.971297
#>              x1         x2 fidelity stage bracket repetition          y
#>           <num>      <num>    <num> <num>   <int>      <num>      <num>
#>               timestamp batch_nr x_domain_x1 x_domain_x2 x_domain_fidelity
#>                  <POSc>    <int>       <num>       <num>             <num>
#>  1: 2026-04-22 10:50:53        1 -3.78874794   8.9439418          0.015625
#>  2: 2026-04-22 10:50:53        1  7.51499556   2.8727705          0.015625
#>  3: 2026-04-22 10:50:53        1  4.01141329  14.2164591          0.015625
#>  4: 2026-04-22 10:50:53        1 -2.64187338   8.1372061          0.015625
#>  5: 2026-04-22 10:50:53        1 -4.88900838   8.1690509          0.015625
#>  6: 2026-04-22 10:50:53        1  1.99590246   4.1789573          0.015625
#>  7: 2026-04-22 10:50:53        1  2.46666083   6.7005370          0.015625
#>  8: 2026-04-22 10:50:53        1 -0.65349133   5.5726678          0.015625
#>  9: 2026-04-22 10:50:53        1  5.99322981   0.4209146          0.015625
#> 10: 2026-04-22 10:50:53        1  6.58782267   6.9898079          0.015625
#> 11: 2026-04-22 10:50:53        1  8.11900991   5.8504708          0.015625
#> 12: 2026-04-22 10:50:53        1 -2.37589060   0.3009783          0.015625
#> 13: 2026-04-22 10:50:53        1 -4.48638001   5.6545639          0.015625
#> 14: 2026-04-22 10:50:53        1 -0.19421404   8.3986926          0.015625
#> 15: 2026-04-22 10:50:53        1  1.03492358  12.8562538          0.015625
#> 16: 2026-04-22 10:50:53        1 -2.06495248   5.7721457          0.015625
#> 17: 2026-04-22 10:50:53        1  1.05307176   7.9187555          0.015625
#> 18: 2026-04-22 10:50:53        1 -4.04507814   9.0095629          0.015625
#> 19: 2026-04-22 10:50:53        1  0.83051970   3.9205704          0.015625
#> 20: 2026-04-22 10:50:53        1  9.63321753   4.3507524          0.015625
#> 21: 2026-04-22 10:50:53        1 -0.65161557   7.2011276          0.015625
#> 22: 2026-04-22 10:50:53        1  5.17570641  13.8000832          0.015625
#> 23: 2026-04-22 10:50:53        1  6.02979398   6.0108028          0.015625
#> 24: 2026-04-22 10:50:53        1 -2.06064900   3.1975907          0.015625
#> 25: 2026-04-22 10:50:53        1  9.70809512  10.0765022          0.015625
#> 26: 2026-04-22 10:50:53        1  6.12282294   0.8792117          0.015625
#> 27: 2026-04-22 10:50:53        1 -4.22830586  14.9560370          0.015625
#> 28: 2026-04-22 10:50:53        1  2.95318695   2.2355320          0.015625
#> 29: 2026-04-22 10:50:53        1  5.43735818   7.7783495          0.015625
#> 30: 2026-04-22 10:50:53        1  5.32834005  12.6918008          0.015625
#> 31: 2026-04-22 10:50:53        1 -4.53154512  10.7740459          0.015625
#> 32: 2026-04-22 10:50:53        1 -1.61656198   3.6197103          0.015625
#> 33: 2026-04-22 10:50:53        1 -0.48753791   8.2056505          0.015625
#> 34: 2026-04-22 10:50:53        1  4.54698422  12.5220272          0.015625
#> 35: 2026-04-22 10:50:53        1  2.18536825   0.4193404          0.015625
#> 36: 2026-04-22 10:50:53        1  1.48256887   7.0407645          0.015625
#> 37: 2026-04-22 10:50:53        1  5.59650757  12.0852005          0.015625
#> 38: 2026-04-22 10:50:53        1  9.22864864  12.2107697          0.015625
#> 39: 2026-04-22 10:50:53        1 -2.29491848   6.0586650          0.015625
#> 40: 2026-04-22 10:50:53        1 -1.74650185   3.2764651          0.015625
#> 41: 2026-04-22 10:50:53        1  5.20244376   6.2754210          0.015625
#> 42: 2026-04-22 10:50:53        1  2.48268416  10.0330612          0.015625
#> 43: 2026-04-22 10:50:53        1  4.62519023   7.6147542          0.015625
#> 44: 2026-04-22 10:50:53        1  4.90426524   9.9053896          0.015625
#> 45: 2026-04-22 10:50:53        1 -3.55963763   7.6768697          0.015625
#> 46: 2026-04-22 10:50:53        1  6.48400246  12.5332866          0.015625
#> 47: 2026-04-22 10:50:53        1  6.54512206  10.6317174          0.015625
#> 48: 2026-04-22 10:50:53        1  9.86068468  13.1130891          0.015625
#> 49: 2026-04-22 10:50:53        1  9.55781354   0.1721931          0.015625
#> 50: 2026-04-22 10:50:53        1  0.83774141  13.3237435          0.015625
#> 51: 2026-04-22 10:50:53        1  1.91779697  14.9452038          0.015625
#> 52: 2026-04-22 10:50:53        1 -0.27137371   7.5028725          0.015625
#> 53: 2026-04-22 10:50:53        1 -2.37986159   5.3845054          0.015625
#> 54: 2026-04-22 10:50:53        1  2.97360311  11.6236953          0.015625
#> 55: 2026-04-22 10:50:53        1  2.40455524   8.7671288          0.015625
#> 56: 2026-04-22 10:50:53        1  6.68962939   9.5096456          0.015625
#> 57: 2026-04-22 10:50:53        1 -1.93732486  12.8799923          0.015625
#> 58: 2026-04-22 10:50:53        1  5.70095918   8.5034151          0.015625
#> 59: 2026-04-22 10:50:53        1 -4.02175832   3.7949553          0.015625
#> 60: 2026-04-22 10:50:53        1  0.31310198  13.7820482          0.015625
#> 61: 2026-04-22 10:50:53        1  7.37799132  13.0102531          0.015625
#> 62: 2026-04-22 10:50:53        1 -0.89272632   3.7280805          0.015625
#> 63: 2026-04-22 10:50:53        1  3.55067427   6.0432182          0.015625
#> 64: 2026-04-22 10:50:53        1  0.03578621  11.5444526          0.015625
#>               timestamp batch_nr x_domain_x1 x_domain_x2 x_domain_fidelity
#>                  <POSc>    <int>       <num>       <num>             <num>
```
