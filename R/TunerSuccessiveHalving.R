#' @title Hyperparameter Tuning with Successive Halving
#'
#' @name mlr_tuners_successive_halving
#' @templateVar id successive_halving
#'
#' @description
#' `TunerSuccessiveHalving` class that implements the successive halving
#' algorithm (SHA). SHA randomly samples `n` candidate
#' hyperparameter configurations and allocates a minimum budget (`r_min`) to all
#' candidates. The candidates are raced down in stages to a single best
#' candidate by repeatedly increasing the budget by a factor of `eta` and
#' promoting only the best `1 / eta ` fraction to the next stage. This means
#' promising hyperparameter configurations are allocated a higher budget overall
#' and lower performing ones are discarded early on.
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage, is set by
#' the lower bound of the budget parameter. The upper bound  defines the maximum
#' budget (`r_max`) which is allocated to the candidates in the last stage. The
#' number of stages is computed so that each candidate in base stage is
#' allocated the minimum budget and the candidates in the last stage are not
#' evaluated on more than the maximum budget. The following table is the stage
#' layout for `eta = 2`, `r_min = 1` and `r_max = 8`.
#'
#' | `i` | `n_i` | `r_i` |
#' | --: | ----: | ----: |
#' |   0 |     8 |     1 |
#' |   1 |     4 |     2 |
#' |   2 |     2 |     4 |
#' |   3 |     1 |     8 |
#'
#' `i` is stage number, `n_i` is the number of configurations and `r_i` is the
#' budget allocated to a single configuration.
#'
#' @section Parameters:
#' \describe{
#' \item{`n`}{`integer(1)`\cr
#' Number of candidates in base stage.
#' }
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1 / eta` candidates are promoted to the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }
#' \item{`repeats`}{`logical(1)`\cr
#' If `FALSE` (default), SHA terminates once all stages are evaluated.
#' Otherwise, SHA starts over again once the last stage is evaluated.
#' }
#' \item{`adjust_minimum_budget`}{`logical(1)`\cr
#' If `TRUE`, minimum budget is increased so that the last stage uses the
#' maximum budget defined in the search space.
#' }}
#'
#' @section Archive:
#' The [mlr3tuning::ArchiveTuning] holds the following additional columns that
#' are specific to the successive halving algorithm:
#'   * `stage` (`integer(1))`\cr
#'     Stage index. Starts counting at 0.
#'   * `repetition` (`integer(1))`\cr
#'     Repetition index. Start counting at 1.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' The hyperparameter configurations of one stage are evaluated in parallel with
#' the \CRANpkg{future} package. To select a parallel backend, use
#' [future::plan()].
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
        optimizer = OptimizerSuccessiveHalving$new()
      )
    }
  )
)
