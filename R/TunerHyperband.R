#' @title Tuner using the Hyperband algorithm
#'
#' @name mlr_tuners_hyperband
#'
#' @description
#' `TunerHyperband` class that implements hyperband tuning. Hyperband is a
#' budget oriented-procedure, weeding out suboptimal performing configurations
#' early in a sequential training process, increasing tuning efficiency as a
#' consequence.
#'
#' For this, several brackets are constructed with an associated set of
#' configurations for each bracket. Each bracket has several stages. Different
#' brackets are initialized with different amounts of configurations and
#' different budget sizes.
#'
#' Within the context of hyperband each evaluation of a learner consumes a
#' certain budget. This budget is not fixed but controlled by a certain
#' hyperparameter, e.g. the number of boosting iterations or the number of trees
#' in a random forest. The user has to specify explicitly which hyperparameter
#' of the learner controls the consumption of the budget by
#' tagging a single hyperparameter in the [paradox::ParamSet] with `"budget"`.
#' An alternative approach using subsampling and pipelines is described below.
#'
#' Naturally, hyperband terminates once all of its brackets are evaluated, so a
#' [bbotk::Terminator] in the tuning instance acts as an upper bound and should
#' be only set to a low value if one is unsure of how long hyperband will take
#' to finish under the given settings.
#' 
#' @templateVar id hyperband
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' Fraction parameter of the successive halving algorithm: With every step the
#' configuration budget is increased by a factor of `eta` and only the best
#' `1/eta` configurations are used for the next stage. Non-integer values are
#' supported, but `eta` is not allowed to be less or equal 1.}
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn during
#' the initialization of each bracket. The default is uniform sampling.}
#' }
#'
#' @section Archive:
#' The [mlr3tuning::ArchiveTuning] holds the following additional columns that
#' are specific to the hyperband tuner:
#'   * `bracket` (`integer(1)`)\cr
#'     The console logs about the bracket index are actually not matching
#'     with the original hyperband algorithm, which counts down the brackets
#'     and stops after evaluating bracket 0. The true bracket indices are
#'     given in this column.
#'   * `bracket_stage` (`integer(1))`\cr
#'     The bracket stage of each bracket. Hyperband starts counting at 0.
#'   * `budget_scaled` (`numeric(1)`)\cr
#'     The intermediate budget in each bracket stage calculated by hyperband.
#'     Because hyperband is originally only considered for budgets starting at 1, some
#'     rescaling is done to allow budgets starting at different values.
#'     For this, budgets are internally divided by the lower budget bound to
#'     get a lower budget of 1. Before the learner
#'     receives its budgets for evaluation, the budget is transformed back to
#'     match the original scale again.
#'   * `budget_real` (`numeric(1)`)\cr
#'     The real budget values the learner uses for evaluation after hyperband
#'     calculated its scaled budget.
#'   * `n_configs` (`integer(1)`)\cr
#'     The amount of evaluated configurations in each stage. These correspond
#'     to the `r_i` in the original paper.
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
#' @template section_runtime
#' @template section_progress_bars
#' @template section_parallelization
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2018")`
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
#' donttest{
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "hyperband",
#'   task = tsk("pima"),
#'   learner = lrn("classif.xgboost", eval_metric = "logloss"),
#'   resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   search_space = search_space
#' )
#' 
#' # best performing hyperparameter configuration
#' instance$result
#' }
#' }
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
