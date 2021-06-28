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
#' configurations for each bracket. Each bracket as several stages. Different
#' brackets are initialized with different amounts of configurations and
#' different budget sizes. To get an idea of how the bracket layout looks like
#' for a given argument set, please have a look in the `details`.
#'
#' To identify the budget for evaluating hyperband, the user has to specify
#' explicitly which hyperparameter of the learner influences the budget by
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
#' @section Custom sampler:
#' Hyperband supports custom [paradox::Sampler] object for initial
#' configurations in each bracket.
#' A custom sampler may look like this (the full example is given in the
#' `examples` section):
#' ```
#' # - beta distribution with alpha = 2 and beta = 5
#' # - categorical distribution with custom probabilities
#' sampler = SamplerJointIndep$new(list(
#'   Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
#'   Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
#' ))
#' ```
#'
#' @section Runtime:
#' The calculation of each bracket currently assumes a linear runtime in the
#' chosen budget parameter is always given. Hyperband is designed so each
#' bracket requires approximately the same runtime as the sum of the budget
#' over all configurations in each bracket is roughly the same. This will not
#' hold true once the scaling in the budget parameter is not linear
#' anymore, even though the sum of the budgets in each bracket remains the
#' same. A basic example can be viewed by calling the function
#' `hyperband_brackets` below with the arguments `R = 2` and `eta = 2`. If we
#' run a learner with O(budget^2) time complexity, the runtime of the last
#' bracket will be 33% longer than the first bracket
#' (time of bracket 1 = 2 * 1^2 + 2^2 = 6; time of bracket 2 = 2 * 2^2 = 8).
#' Of course, this won't break anything, but it should be kept in mind when
#' applying hyperband. A possible adaption would be to introduce a trafo,
#' like it is shown in the `examples`.
#'
#' @details
#' This sections explains the calculation of the constants for each bracket.
#' A small overview will be given here, but for more details please check
#' out the original paper (see `references`).
#' To keep things uniform with the notation in the paper (and to safe space in
#' the formulas), `R` is used for the upper budget that last remaining
#' configuration should reach. The formula to calculate the amount of brackets
#' is `floor(log(R, eta)) + 1`. To calculate the starting budget in each
#' bracket, use `R * eta^(-s)`, where `s` is the maximum bracket minus the
#' current bracket index.
#' For the starting configurations in each bracket it is
#' `ceiling((B/R) * ((eta^s)/(s+1)))`, with `B = (bracket amount) * R`.
#' To receive a table with the full brackets layout, load the following function
#' and execute it for the desired `R` and `eta`.
#'
#' ```
#' hyperband_brackets = function(R, eta) {
#'
#'   result = data.frame()
#'   smax = floor(log(R, eta))
#'   B = (smax + 1) * R
#'
#'   # outer loop - iterate over brackets
#'   for (s in smax:0) {
#'
#'     n = ceiling((B/R) * ((eta^s)/(s+1)))
#'     r = R * eta^(-s)
#'
#'     # inner loop - iterate over bracket stages
#'     for (i in 0:s) {
#'
#'       ni = floor(n * eta^(-i))
#'       ri = r * eta^i
#'       result = rbind(result, c(smax - s + 1, i + 1, ri, ni))
#'     }
#'   }
#'
#'   names(result) = c("bracket", "bracket_stage", "budget", "n_configs")
#'   return(result)
#' }
#'
#' hyperband_brackets(R = 81L, eta = 3L)
#' ```
#'
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
