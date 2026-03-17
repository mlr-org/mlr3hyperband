# Asynchronous Hyperparameter Optimization with Successive Halving

`OptimizerAsyncSuccessiveHalving` class that implements the Asynchronous
Successive Halving Algorithm (ASHA). This class implements the
asynchronous version of
[OptimizerBatchSuccessiveHalving](https://mlr3hyperband.mlr-org.com/reference/mlr_optimizers_successive_halving.md).

## Source

Li L, Jamieson K, Rostamizadeh A, Gonina E, Ben-tzur J, Hardt M, Recht
B, Talwalkar A (2020). “A System for Massively Parallel Hyperparameter
Tuning.” In Dhillon I, Papailiopoulos D, Sze V (eds.), *Proceedings of
Machine Learning and Systems*, volume 2, 230–246.
<https://proceedings.mlsys.org/paper_files/paper/2020/hash/a06f20b349c6cf09a6b171c71b88bbfc-Abstract.html>.

## Dictionary

This
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[bbotk::mlr_optimizers](https://bbotk.mlr-org.com/reference/mlr_optimizers.html)
or with the associated sugar function
[`bbotk::opt()`](https://bbotk.mlr-org.com/reference/opt.html):

    mlr_optimizers$get("async_successive_halving")
    opt("async_successive_halving")

## Parameters

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

## Archive

The [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html)
holds the following additional columns that are specific to SHA:

- `stage` (`integer(1))`  
  Stage index. Starts counting at 0.

- `asha_id` (`character(1))`  
  Unique identifier for each configuration across stages.

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

## Super classes

[`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
-\>
[`bbotk::OptimizerAsync`](https://bbotk.mlr-org.com/reference/OptimizerAsync.html)
-\> `OptimizerAsyncSuccessiveHalving`

## Methods

### Public methods

- [`OptimizerAsyncSuccessiveHalving$new()`](#method-OptimizerAsyncSuccessiveHalving-new)

- [`OptimizerAsyncSuccessiveHalving$optimize()`](#method-OptimizerAsyncSuccessiveHalving-optimize)

- [`OptimizerAsyncSuccessiveHalving$clone()`](#method-OptimizerAsyncSuccessiveHalving-clone)

Inherited methods

- [`bbotk::Optimizer$format()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-format)
- [`bbotk::Optimizer$help()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-help)
- [`bbotk::Optimizer$print()`](https://bbotk.mlr-org.com/reference/Optimizer.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OptimizerAsyncSuccessiveHalving$new()

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the optimization on a
[bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
or
[bbotk::OptimInstanceAsyncMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncMultiCrit.html)
until termination. The single evaluations will be written into the
[bbotk::ArchiveAsync](https://bbotk.mlr-org.com/reference/ArchiveAsync.html).
The result will be written into the instance object.

#### Usage

    OptimizerAsyncSuccessiveHalving$optimize(inst)

#### Arguments

- `inst`:

  ([bbotk::OptimInstanceAsyncSingleCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
  \|
  [bbotk::OptimInstanceAsyncMultiCrit](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncMultiCrit.html)).

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerAsyncSuccessiveHalving$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
