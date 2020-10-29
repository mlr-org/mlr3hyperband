#' @title Tuner using the Successive Halving
#'
#' @name mlr_tuners_successive_halving
#'
#' @description
#' Successive Halving Algorithm.
#'
#' @section Parameters:
#' \describe{
#' \item{`n`}{`integer(1)`\cr
#' Number of configurations in first stage.}
#' \item{`eta`}{`numeric(1)`\cr
#' With every step the configuration budget is increased by a factor of `eta`
#' and only the best `1/eta` configurations are used for the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.}
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn during
#' the initialization of each bracket. The default is uniform sampling.}
#' }
#'
#' @export
TunerSuccessiveHalving = R6Class("TunerSuccessiveHalving",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("n", lower = 1),
        ParamDbl$new("eta", lower = 1.0001),
        ParamUty$new("sampler",
          custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE))
      ))
      ps$values = list(eta = 2, sampler = NULL)

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
      budget_id = ps$ids(tags = "budget")

      ps_sampler = ps$clone()$subset(setdiff(ps$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(ps_sampler)
      }

      r_min = ps$lower[[budget_id]]
      r_max = ps$upper[[budget_id]]

      # Number of stages if each configuration in the fist stage uses r_min
      # resources and each configuration in the last stage uses not more than
      # r_max resources
      k_n = floor(log(r_max / r_min, eta))

      # Number of stages so that the last stages evaluates more than 1
      # configuration
      k_r = floor(log(n, eta))

      k = min(k_n, k_r)

      for (i in 0:k) {
        ni = ceiling(n * eta^(-i)) # Number of configurations in stage
        ri = r_min * eta^i # Resources of each configuration in stage

        if (i == 0) {
          xdt = sampler$sample(ni)$data
          xdt$continue_hash = seq(nrow(xdt))
        } else {
          archive = inst$archive
          data = archive$data()[batch_nr %in% archive$n_batch]
          y = data[, archive$cols_y, with = FALSE]
          minimize = !as.logical(mult_max_to_min(archive$codomain))

          if (archive$codomain$length == 1) {
            row_ids = head(order(y, decreasing = minimize), ni)
          } else {
            row_ids = nds_selection(points = t(as.matrix(y)), n_select = ni,
              minimize = minimize)
          }
          xdt = data[row_ids, c(archive$cols_x, "continue_hash"), with = FALSE]
        }
        xdt[[budget_id]] = ri
        xdt$stage = i + 1
        inst$eval_batch(xdt)
      }
    }
  )
)
