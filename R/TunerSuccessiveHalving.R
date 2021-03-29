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
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("n", lower = 1, default = 16),
        ParamDbl$new("eta", lower = 1.0001, default = 2),
        ParamUty$new("sampler", custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE))
      ))
      ps$values = list(n = 16L, eta = 2L, sampler = NULL)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = ps,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = "emoa"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      n = pars$n
      eta = pars$eta
      sampler = pars$sampler
      ps = inst$search_space
      budget_id = ps$ids(tags = "retrain")

      ps_sampler = ps$clone()$subset(setdiff(ps$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(ps_sampler)
      }

      r_min = ps$lower[[budget_id]]
      r_max = ps$upper[[budget_id]]

      # number of stages if each configuration in the fist stage uses r_min resources
      # and each configuration in the last stage uses not more than r_max resources
      k_n = floor(log(r_max / r_min, eta))

      # number of stages so that the last stages evaluates at least one configuration
      k_r = floor(log(n, eta))

      k = min(k_n, k_r)

      for (i in 0:k) {
        ni = floor(n * eta^(-i)) # number of configurations in stage
        ri = r_min * eta^i # resources of each configuration in stage

        if (i == 0) {
          xdt = sampler$sample(ni)$data
        } else {
          archive = inst$archive
          data = archive$data[batch_nr %in% archive$n_batch]
          y = data[, archive$cols_y, with = FALSE]
          minimize = !as.logical(mult_max_to_min(archive$codomain))

          if (archive$codomain$length == 1) {
            row_ids = head(order(y, decreasing = minimize), ni)
          } else {
            row_ids = nds_selection(points = t(as.matrix(y)), n_select = ni, minimize = minimize)
          }
          xdt = data[row_ids, archive$cols_x, with = FALSE]
        }
        xdt[[budget_id]] = ri
        xdt$stage = i + 1
        inst$eval_batch(xdt)
      }
    }
  )
)
