#' @title Tuner using the Hyperband algorithm
#'
#' @name mlr_tuners_hyperband
#' @templateVar id hyperband
#'
#' @description
#' `TunerHyperband` class that implements hyperband tuning (HBX). HBX
#' repeatedly calls SHA ([TunerSuccessiveHalving]) with different numbers of
#' starting configurations. A larger number of starting configurations
#' corresponds to a smaller budget allocated in the base stage. Each run of SHA
#' within HBX is called a bracket. HBX considers `s_max + 1` brackets with
#' `s_max = floor(log(r_max / r_min, eta)`. The most explorative bracket
#' `s = s_max` constructs `s_max + 1` stages and allocates the minimum budget
#' (`r_min`) in the base stage. The minimum budget is increased in each bracket
#' by a factor of `eta` and the number of starting configurations is computed so
#' that each bracket approximately spends the same budget. Use
#' [hyperband_schedule()] to get a preview of the bracket layout.
#'
#' | `s` |     |       |     3 |     |       |     2 |     |       |     1 |     |       |     0 |
#' | --: | --- | ----: | ----: | --- | ----: | ----: | --- | ----: | ----: | --- | ----: | ----: |
#' | `i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |
#' |   0 |     |     8 |     1 |     |     6 |     2 |     |     4 |     4 |     |     8 |     4 |
#' |   1 |     |     4 |     2 |     |     3 |     4 |     |     2 |     8 |     |       |       |
#' |   2 |     |     2 |     4 |     |     1 |     8 |     |       |       |     |       |       |
#' |   3 |     |     1 |     8 |     |       |       |     |       |       |     |       |       |
#'
#' `s` is the bracket number, `i` is stage number, `n_i` is the number of
#' configurations and `r_i` is the budget allocated to a single configuration.
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most
#' explorative bracket, is set by the lower bound of the budget parameter. The
#' upper bound defines the maximum budget (`r_max`) which which is allocated to
#' the candidates in the last stages.
#'
#' @templateVar id hyperband
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1 / eta` configurations are promoted to the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn in the
#' base stage of each bracket. The default is uniform sampling.
#' }
#' \item{`repetitions`}{`integer(1)`\cr
#' If `1` (default), optimization is stopped once all brackets are evaluated.
#' Otherwise, optimization is stopped after `repetitions` runs of hyperband. The
#' [bbotk::Terminator] might stop the optimization before all repetitions are
#' executed.
#' }}
#'
#' @section Archive:
#' The [mlr3tuning::ArchiveTuning] holds the following additional columns that
#' are specific to the hyperband algorithm:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'   * `repetition` (`integer(1))`\cr
#'     Repetition index. Start counting at 1.
#'
#' @section Hyperband without learner budget:
#' Thanks to \CRANpkg{mlr3pipelines}, it is possible to use hyperband in
#' combination with learners lacking a natural budget parameter. For example,
#' any [mlr3::Learner] can be augmented with a [mlr3pipelines::PipeOp]
#' operator such as [mlr3pipelines::PipeOpSubsample]. With the
#' subsampling rate as budget parameter, the resulting
#' [mlr3pipelines::GraphLearner] is fitted on small proportions of
#' the [mlr3::Task] in the first brackets, and on the complete Task in
#' last brackets. See examples for some code.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' This hyperband implementation evaluates hyperparameter configurations of
#' equal budget across brackets in one batch. For example, all configurations
#' in stage 1 of bracket 3 and stage 0 of bracket 2 in one batch. To select a
#' parallel backend, use [future::plan()].
#'
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2018")`
#'
#' @export
#' @template example_tuner
TunerHyperband = R6Class("TunerHyperband",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerHyperband$new()
      )
    }
  )
)
