#' @title Tuner Asynchronous Successive Halving
#'
#' @name mlr_tuners_asha
#'
#' @description
#' `TunerAsha` class that implements the asynchronous successive halving
#' algorithm. Asynchronous successive halving (ASHA) parallelizes SHA
#' ([TunerSuccessiveHalving]) by promoting candidates to the next stage as soon
#' as possible instead of waiting for all candidates in the stage to finish.
#' ASHA starts with sampling a candidate hyperparameter configuration for each
#' available worker. When an evaluation finishes and the worker is available
#' again, ASHA checks the stages from top to bottom for promotable candidates.
#' Promotions are possible when the evaluated candidates belong to the top
#' `1 / eta` of each stage. If no promotions are possible, a new candidate is
#' sampled and added to the base stage, which increases the number of possible
#' promotions for all stages.
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage, is set by
#' the lower bound of the budget parameter. The upper bound defines the maximum
#' budget (`r_max`) which which is allocated to the candidates in the last
#' stage.
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
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional column that is specific
#' to the successive halving algorithm:
#'   * `stage` (`integer(1))`\cr
#'     Stage index. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' The hyperparameter configurations are asynchronously evaluated with the
#' \CRANpkg{future} package. The resampling of each candidate is send to an
#' available worker. To select a parallel backend, use [future::plan()].
#'
#' @template section_logging
#'
#' @export
#' @examples
#' if(requireNamespace("xgboost")) {
#' library(mlr3learners)
#'
#' # define hyperparameter and budget parameter
#' search_space = ps(
#'   nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
#'   eta = p_dbl(lower = 0, upper = 1),
#'   booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
#' )
#'
#' \donttest{
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "asha",
#'   task = tsk("pima"),
#'   learner = lrn("classif.xgboost", eval_metric = "logloss"),
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   search_space = search_space,
#'   term_evals = 100
#' )
#'
#' # best performing hyperparameter configuration
#' instance$result
#' }
#' }
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
