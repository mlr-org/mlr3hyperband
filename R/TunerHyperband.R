#' @title Tuner Using the Hyperband Algorithm
#'
#' @include OptimizerHyperband.R
#' @name mlr_tuners_hyperband
#' @templateVar id hyperband
#'
#' @inherit mlr_optimizers_hyperband description
#' @inheritSection mlr_optimizers_hyperband Resources
#' @template section_dictionary_tuners
#' @inheritSection mlr_optimizers_hyperband Parameters
#' @inheritSection mlr_optimizers_hyperband Archive
#' @template section_subsample_budget
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' This hyperband implementation evaluates hyperparameter configurations of equal budget across brackets in one batch.
#' For example, all configurations in stage 1 of bracket 3 and stage 0 of bracket 2 in one batch.
#' To select a parallel backend, use the `plan()` function of the \CRANpkg{future} package.
#'
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2018")`
#'
#' @export
#' @template example_tuner
TunerHyperband = R6Class("TunerHyperband",
  inherit = TunerBatchFromOptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerHyperband$new(),
        man = "mlr3hyperband::mlr_tuners_hyperband"
      )
    }
  )
)

#' @include aaa.R
tuners[["hyperband"]] = TunerHyperband
