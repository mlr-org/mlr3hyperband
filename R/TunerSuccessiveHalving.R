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
#'   method = "successive_halving",
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
