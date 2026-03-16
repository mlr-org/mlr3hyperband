# Changelog

## mlr3hyperband (development version)

- feat: The mlr3 ecosystem has a base logger now which is named `mlr3`.
  The `mlr3/bbotk` logger is a child of the `mlr3` logger and is used
  for logging messages from the `bbotk`, `mlr3tuning` and
  `mlr3hyperband` package.
- compatibility: Compatible with `rush` 1.0.0.

## mlr3hyperband 1.0.0

CRAN release: 2025-07-10

- feat: Add `OptimizerAsyncSuccessiveHalving` optimizer.

## mlr3hyperband 0.6.0

CRAN release: 2024-06-29

- compatibility: Work with new bbotk 1.0.0 and mlr3tuning 1.0.0

## mlr3hyperband 0.5.0

CRAN release: 2024-03-05

- compatibility: Work with new paradox version 1.0.0

## mlr3hyperband 0.4.5

CRAN release: 2023-03-02

- fix: Unloading `mlr3hyperband` removes optimizers and tuners from the
  dictionaries.
- docs: Update resources and descriptions.
- tests: Remove deprecated `method` argument of
  [`mlr3tuning::tune()`](https://mlr3tuning.mlr-org.com/reference/tune.html).

## mlr3hyperband 0.4.4

CRAN release: 2022-11-27

- fix: Remove `emoa` from required packages of
  `OptimizerBatchSuccessiveHalving`.

## mlr3hyperband 0.4.3

CRAN release: 2022-11-07

- docs: Examples use
  [`branin_wu()`](https://bbotk.mlr-org.com/reference/branin.html)
  function now.

## mlr3hyperband 0.4.2

CRAN release: 2022-08-25

- docs: Re-generate rd files with valid html.

## mlr3hyperband 0.4.1

CRAN release: 2022-05-04

- feat: `Optimizer` and `Tuner` objects have the field `$id` now.
- feat: `Optimizer` and `Tuner` objects have the optional field `$label`
  now.
- feat: New `$help()` method which opens the manual page of `Optimizer`
  and `Tuner` objects.

## mlr3hyperband 0.4.0

CRAN release: 2022-02-10

- feat: New `adjust_minimum_budget` flag in
  `OptimizerBatchSuccessiveHalving`. The minimum budget is adjusted in
  the base stage to use the maximum budget in last stage.
- feat: New `repetitions` parameter to specify the exact number of
  repetitions. Replaced the `repeats` parameter.

## mlr3hyperband 0.3.0

CRAN release: 2022-01-23

- feat: `TunerBatchHyperband` evaluates configurations of same budget
  across brackets in parallel now.
- feat: New `repeats` parameter to repeat runs of successive halving and
  hyperband until termination.
- fix: Bug where maximization measures were minimized.

## mlr3hyperband 0.2.0

CRAN release: 2021-09-13

- feat: New `OptimizerBatchHyperband` and
  `OptimizerBatchSuccessiveHalving` optimizers.

## mlr3hyperband 0.1.2

CRAN release: 2021-01-29

- refactor: Make XGBoost conditionally in examples.

## mlr3hyperband 0.1.1

CRAN release: 2020-12-07

- fix: Dependency on `emoa`.

## mlr3hyperband 0.1.0

CRAN release: 2020-10-26

- Initial CRAN release.
