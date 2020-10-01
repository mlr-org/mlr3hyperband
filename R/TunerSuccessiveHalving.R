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
#' Maximum number of configuration.}
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
      r = pars$r
      sampler = pars$sampler
      ps = inst$search_space
      budget_id = ps$ids(tags = "budget")

      ps_sampler = ps$clone()$subset(setdiff(ps$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(ps_sampler)
      }

      budget_lower = ps$lower[[budget_id]]
      budget_upper = ps$upper[[budget_id]]

      # Rescale budget
      r = budget_upper/budget_lower
      r = r * eta^(-smax) # Budget allocated to the first configurations

      smax = floor(log(n, eta))

      for(stage in 0:smax) {
        ni = floor(n*eta^(-stage)) # Number of configurations in stage
        ri = r * eta^stage # Budget of each configuration
        ri_real = floor(ri*budget_lower) # Rescale budget back

        if(stage == 0) {
          xdt = sampler$sample(ni)$data
          xdt[[budget_id]] = ri_real
          xdt$continue_hash = seq(nrow(xdt))
          xdt$stage = stage + 1
        } else {
          archive = inst$archive$data()
          setorderv(archive, cols = inst$archive$cols_y)
          archive = head(archive, ni)
          xdt = archive[,c(inst$archive$cols_x, "continue_hash"), with = FALSE]
          xdt[[budget_id]] = ri_real
          xdt$stage = stage + 1
        }
        inst$eval_batch(xdt)
      }
    }
  )
)
