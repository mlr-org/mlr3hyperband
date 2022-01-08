#' @title Tuner Asynchronous Hyperband
#'
#' @name mlr_tuners_ahb
#' @templateVar id ahb
#'
#' @description
#' `TunerAhb` class that implements the asynchronous hyperband algorithm (AHB).
#' AHB repeatedly runs ASHA ([TunerAsha]) with different minimum budgets in the
#' base stage. Each run of ASHA within AHB is called a bracket. AHB considers
#' `s_max + 1` brackets with `s_max = floor(log(r_max / r_min, eta)`. The most
#' explorative bracket `s = s_max` constructs `s_max + 1` stages and allocates
#' the minimum budget (`r_min`) in the base stage.  The minimum budget (`r_min`)
#' is increased in each bracket by a factor of `eta` until the maximum budget is
#' allocated in the base stage. The bracket `s = 0` is a random search with full
#' budget. Each ASHA run uses `1 / s_max + 1` of the [bbotk::Terminator].
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most
#' explorative bracket, is set by the lower bound of the budget parameter. The
#' upper bound defines the maximum budget (`r_max`) which which is allocated to
#' the candidates in the last stages.
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1 / eta` candidates are promoted to the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn in the
#' base stage of each bracket. The default is uniform sampling.
#' }
#' \item{`adjust_minimum_budget`}{`logical(1)`\cr
#' If `TRUE`, minimum budget is increased so that the last stage uses the
#' maximum budget defined in the search space.
#' }}
#'
#' @section Archive:
#' The [mlr3tuning::ArchiveTuning] holds the following additional columns that
#' are specific to the hyperband algorithm:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' The hyperparameter configurations are asynchronously evaluated with
#' the \CRANpkg{future} package. The resampling of each candidate is send to an
#' available worker. To select a parallel backend, use [future::plan()].
#'
#' @template section_logging
#'
#' @export
#' @template example_tuner
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
