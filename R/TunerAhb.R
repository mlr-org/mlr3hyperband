#' @title Tuner Asynchronous Hyperband
#'
#' @name mlr_tuners_ahb
#'
#' @description
#' `TunerAhb` class that implements the asynchronous hyperband algorithm.
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta` and only the
#' best `1/eta` points are promoted for the next stage. Non-integer values are
#' supported, but `eta` is not allowed to be less or equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional columns that are specific
#' to the hyperband algorithm:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Starts counting at 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_runtime
#' @template section_progress_bars
#' @template section_logging
#'
#' @export
TunerAhb = R6Class("TunerAhb",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAhb$new()
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
