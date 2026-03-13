# Asynchronous Hyperparameter Tuning with Successive Halving

`OptimizerAsyncSuccessiveHalving` class that implements the Asynchronous
Successive Halving Algorithm (ASHA). This class implements the
asynchronous version of
[OptimizerBatchSuccessiveHalving](https://mlr3hyperband.mlr-org.com/dev/reference/mlr_optimizers_successive_halving.md).

## Source

Li L, Jamieson K, Rostamizadeh A, Gonina E, Ben-tzur J, Hardt M, Recht
B, Talwalkar A (2020). “A System for Massively Parallel Hyperparameter
Tuning.” In Dhillon I, Papailiopoulos D, Sze V (eds.), *Proceedings of
Machine Learning and Systems*, volume 2, 230–246.
<https://proceedings.mlsys.org/paper_files/paper/2020/hash/a06f20b349c6cf09a6b171c71b88bbfc-Abstract.html>.

## Dictionary

This
[mlr3tuning::Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3tuning::mlr_tuners](https://mlr3tuning.mlr-org.com/reference/mlr_tuners.html)
or with the associated sugar function
[`mlr3tuning::tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.html):

    TunerAsyncSuccessiveHalving$new()
    mlr_tuners$get("async_successive_halving")
    tnr("async_successive_halving")

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

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.html)
-\>
[`mlr3tuning::TunerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsync.html)
-\>
[`mlr3tuning::TunerAsyncFromOptimizerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.html)
-\> `TunerAsyncSuccessiveHalving`

## Methods

### Public methods

- [`TunerAsyncSuccessiveHalving$new()`](#method-TunerAsyncSuccessiveHalving-new)

- [`TunerAsyncSuccessiveHalving$clone()`](#method-TunerAsyncSuccessiveHalving-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)
- [`mlr3tuning::TunerAsyncFromOptimizerAsync$optimize()`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerAsyncSuccessiveHalving$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsyncSuccessiveHalving$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
