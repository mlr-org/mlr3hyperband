#' @title Tuner Asynchronous Successive Halving
#'
#' @name mlr_tuners_asha
#'
#' @description
#' `TunerAsha` class that implements the asynchronous successive halving
#' algorithm.
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta` and only the
#' best `1/eta` configurations are promoted for the next stage. Non-integer
#' values are supported, but `eta` is not allowed to be less or equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }
#' \item{`s`}{`integer(1)`\cr
#' Minimum early-stopping rate.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional column that is specific
#' to the successive halving algorithm:
#'   * `stage` (`integer(1))`\cr
#'     Stage index. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_runtime
#' @template section_progress_bars
#' @template section_logging
#'
#' @export
TunerAsha = R6Class("TunerAsha",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsha$new()
      )
    },

    #' @description
    #' Performs the tuning on a [TuningInstanceSingleCrit] /
    #' [TuningInstanceMultiCrit] until termination. The single evaluations and
    #' the final results will be written into the [ArchiveTuning] that
    #' resides in the [TuningInstanceSingleCrit]/[TuningInstanceMultiCrit].
    #' The final result is returned.
    #'
    #' @param inst ([TuningInstanceSingleCrit] | [TuningInstanceMultiCrit]).
    #'
    #' @return [data.table::data.table].
    optimize = function(inst) {
      inst$async = TRUE
      super$optimize(inst)
    }
  )
)
