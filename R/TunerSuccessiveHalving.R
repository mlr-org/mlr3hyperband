#' @title Hyperparameter Tuning with Successive Halving
#'
#' @name mlr_tuners_successive_halving
#'
#' @description
#' `TunerSuccessiveHalving` class that implements the successive halving
#' algorithm.
#' 
#' @section Parameters:
#' \describe{
#' \item{`n`}{`integer(1)`\cr
#' Number of configurations in first stage.}
#' \item{`eta`}{`numeric(1)`\cr
#' With every step, the configuration budget is increased by a factor of `eta`
#' and only the best `1/eta` configurations are used for the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.}
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn during
#' the initialization of each bracket. The default is uniform sampling.}
#' }
#' 
#' @source
#' `r format_bib("jamieson_2016")`
#'
#' @export
#' @examples
#' if(requireNamespace("xgboost")) {
#' library(mlr3hyperband)
#' library(mlr3learners)
#'
#' # Define hyperparameter and budget parameter for tuning with hyperband
#' search_space = ParamSet$new(list(
#'   ParamInt$new("nrounds", lower = 1, upper = 4, tag = "budget"),
#'   ParamDbl$new("eta", lower = 0, upper = 1),
#'   ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
#' ))
#'
#' # Define termination criterion
#' # Successive halving terminates itself
#' terminator = trm("none")
#'
#' # Create tuning instance
#' inst = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.xgboost"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = terminator,
#'   search_space = search_space,
#' )
#'
#' # Load tuner
#' tuner = tnr("successive_halving", n = 16L, eta = 2L)
#'
#' \donttest{
#' # Trigger optimization
#' tuner$optimize(inst)
#'
#' # Print all evaluations
#' as.data.table(inst$archive)}
#' }
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
