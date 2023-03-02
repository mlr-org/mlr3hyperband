#' @title Hyperparameter Tuning with Successive Halving
#'
#' @include OptimizerSuccessiveHalving.R
#' @name mlr_tuners_successive_halving
#' @templateVar id successive_halving
#'
#' @inherit mlr_optimizers_successive_halving description
#' @inheritSection mlr_optimizers_successive_halving Resources
#' @template section_dictionary_tuners
#' @inheritSection mlr_optimizers_successive_halving Parameters
#' @inheritSection mlr_optimizers_successive_halving Archive
#' @template section_subsample_budget
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' The hyperparameter configurations of one stage are evaluated in parallel with the \CRANpkg{future} package.
#' To select a parallel backend, use the `plan()` function of the \CRANpkg{future} package.
#'
#' @template section_logging
#'
#' @source
#' `r format_bib("jamieson_2016")`
#'
#' @export
#' @template example_tuner
TunerSuccessiveHalving = R6Class("TunerSuccessiveHalving",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerSuccessiveHalving$new(),
        man = "mlr3hyperband::mlr_tuners_hyperband"
      )
    }
  )
)

#' @include aaa.R
tuners[["successive_halving"]] = TunerSuccessiveHalving
