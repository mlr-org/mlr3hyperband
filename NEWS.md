# mlr3hyperband 0.4.0
* feat: New `adjust_minimum_budget` flag in  `OptimizerSuccessiveHalving`. The
  minimum budget is adjusted in the base stage to use the maximum budget in last
  stage.
* feat: New `repetitions` parameter to specify the exact number of repetitions.
  Replaced the `repeats` parameter.

# mlr3hyperband 0.3.0

* feat: `TunerHyperband` evaluates configurations of same budget across
  brackets in parallel now.
* feat: New `repeats` parameter to repeat runs of successive halving and
  hyperband until termination.
* fix: Bug where maximization measures were minimized.

# mlr3hyperband 0.2.0

* feat: New `OptimizerHyperband` and `OptimizerSuccessiveHalving` optimizers.

# mlr3hyperband 0.1.2

* refactor: Make xgboost conditionally in examples.

# mlr3hyperband 0.1.1

* fix: Dependency on `emoa`.

# mlr3hyperband 0.1.0

* Initial CRAN release.
