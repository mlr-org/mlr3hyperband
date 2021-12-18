#' @title Tuner using the Hyperband algorithm
#'
#' @name mlr_tuners_hyperband_legacy
#'
#' @description
#' `TunerHyperband` class that implements hyperband tuning. Hyperband (HB)
#' repeatedly calls SH ([TunerSuccessiveHalving]) with different numbers of
#' starting configurations. A larger number of starting configurations
#' corresponds to a smaller budget allocated in the base stage. Each run of SH
#' within HBX is called a bracket. HBX considers `s_max + 1` brackets with
#' `s_max = floor(log(r_max / r_min, eta)`. The most explorative bracket
#' `s = s_max` constructs `s_max + 1` stages and allocates the minimum budget
#' (`r_min`) in the base stage. The minimum budget is increased in each bracket
#' by a factor of `eta` and the number of starting configurations is computed so
#' that each bracket approximately spends the same budget.Use
#' [hyperband_schedule()] to get a preview of the bracket layout.
#'
#' |   s |     |   3 |     |     |   2 |     |     |   1 |     |     |   0 |
#' | ---:| ---:| ---:| --- | ---:| ---:| --- | ---:| ---:| --- | ---:| ---:|
#' |   i |  ni |  ri |     |  ni |  ri |     |  ni |  ri |     |  ni |  ri |
#' |   0 |   8 |   1 |     |   6 |   2 |     |   4 |   4 |     |   8 |   4 |
#' |   1 |   4 |   2 |     |   3 |   4 |     |   2 |   8 |     |     |     |
#' |   2 |   2 |   4 |     |   1 |   8 |     |     |     |     |     |     |
#' |   3 |   1 |   8 |     |     |     |     |     |     |     |     |     |
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most
#' explorative bracket, is set by the lower bound of the budget parameter. The
#' upper bound defines the maximum budget (`r_max`) which which is allocated to
#' the candidates in the last stages.
#'
#' @templateVar id hyperband_legacy
#' @template section_dictionary_optimizers
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
#' \item{`repeats`}{`integer(1)`\cr
#' If `1` (default), optimization is stopped once all stages are evaluated.
#' Otherwise, optimization is stopped after `repeats` runs of SHA. The
#' [bbotk::Terminator] might stop the optimization before all repeats are
#' executed.
#' }}
#'
#' @section Archive:
#' The [mlr3tuning::ArchiveTuning] holds the following additional columns that are specific
#' to the hyperband algorithm:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' HB parallelizes the original hyperband algorithm by evaluating
#' hyperparameter configurations of equal budget across brackets in one batch.
#' For example, all configurations in stage 1 of bracket 3 and stage 0 of
#' bracket 2 in one batch. To select a parallel backend, use [future::plan()].
#'
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2018")`
#'
#' @noRd
TunerHyperbandLegacy = R6Class("TunerHyperbandLegacy",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerHyperbandLegacy$new()
      )
    }
  )
)
