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
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("n", lower = 1), # Number of configurations in first stage
        ParamDbl$new("eta", lower = 1.0001),
        ParamUty$new("sampler",
          custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE))
      ))
      ps$values = list(sampler = NULL)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = ps,
        properties = c("dependencies", "single-crit"),
        packages = character(0)
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      n = pars$n
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
      # resources and each configuration in the last stage uses less than r_max
      # resources
      k = floor(log(r_max/r_min, eta))

      for(i in 0:k) {
        ni = floor(n*eta^(-i)) # Number of configurations in stage
        ri = r_min*eta^i # Resources of each configuration in stage

        if(i == 0) {
          xdt = sampler$sample(ni)$data
          xdt[[budget_id]] = ri
          xdt$continue_hash = seq(nrow(xdt))
          xdt$stage = i + 1
        } else {
          archive = inst$archive$data()
          setorderv(archive, cols = inst$archive$cols_y)
          archive = head(archive, ni)
          xdt = archive[,c(inst$archive$cols_x, "continue_hash"), with = FALSE]
          xdt[[budget_id]] = ri
          xdt$stage = i + 1
        }
        inst$eval_batch(xdt)
      }
    }
  )
)
