#' @title Optimizer using the Hyperband algorithm
#'
#' @name mlr_optimizers_hyperband
#'
#' @description
#' `OptimizerHyperband` class that implements hyperband optimization. Hyperband
#' is a budget oriented-procedure, weeding out suboptimal performing
#' points early in a sequential training process, increasing
#' optimization efficiency as a consequence.
#'
#' For this, several brackets are constructed with an associated set of
#' points for each bracket. Each bracket as several stages. Different
#' brackets are initialized with different amounts of points and
#' different budget sizes. To get an idea of how the bracket layout looks like
#' for a given argument set, please have a look in the `details`.
#'
#' To identify the budget for evaluating hyperband, the user has to specify
#' explicitly which parameter of the objective function influences the budget by
#' tagging a single parameter in the [paradox::ParamSet] with `"budget"`.
#'
#' Naturally, hyperband terminates once all of its brackets are evaluated, so a
#' [bbotk::Terminator] in the [OptimInstanceSingleCrit] |
#' [OptimInstanceMultiCrit] acts as an upper bound and should be only set to a
#' low value if one is unsure of how long hyperband will take to finish under
#' the given settings.
#'
#' @templateVar id hyperband
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' Fraction parameter of the successive halving algorithm: With every stage the
#' configuration budget is increased by a factor of `eta` and only the best
#' `1/eta` points are used for the next stage. Non-integer values are
#' supported, but `eta` is not allowed to be less or equal 1.}
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn during
#' the initialization of each bracket. The default is uniform sampling.}
#' }
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional columns that are specific
#' to the hyperband algorithm:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_runtime
#' @template section_progress_bars
#' @template section_parallelization
#' @template section_logging
#
#' @source
#' `r format_bib("li_2018")`
#'
#' @export
#' @examples
#' library(bbotk)
#' library(data.table)
#'
#' search_space = domain = ps(
#'   x1 = p_dbl(-5, 10),
#'   x2 = p_dbl(0, 15),
#'   fidelity = p_dbl(1e-2, 1, tags = "budget")
#' )
#'
#' # modified branin function
#' objective = ObjectiveRFunDt$new(
#'   fun = function(xdt) {
#'     a = 1
#'     b = 5.1 / (4 * (pi ^ 2))
#'     c = 5 / pi
#'     r = 6
#'     s = 10
#'     t = 1 / (8 * pi)
#'     data.table(y =
#'       (a * ((xdt[["x2"]] -
#'       b * (xdt[["x1"]] ^ 2L) +
#'       c * xdt[["x1"]] - r) ^ 2) +
#'       ((s * (1 - t)) * cos(xdt[["x1"]])) +
#'       s - (5 * xdt[["fidelity"]] * xdt[["x1"]])))
#'   },
#'   domain = domain,
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#' )
#'
#' instance = OptimInstanceSingleCrit$new(
#'   objective = objective,
#'   search_space = search_space,
#'   terminator = trm("none")
#' )
#'
#' optimizer = opt("hyperband")
#'
#' # modifies the instance by reference
#' optimizer$optimize(instance)
#'
#' # best scoring evaluation
#' instance$result
#'
#' # all evaluations
#' as.data.table(instance$archive)
OptimizerHyperband = R6Class("OptimizerHyperband",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta     = p_dbl(lower = 1.0001, tags = "required", default = 2),
        sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE))
      )
      param_set$values = list(eta = 2, sampler = NULL)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = param_set,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = character(0)
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      eta = pars$eta
      sampler = pars$sampler
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))

      if (inst$archive$codomain$length > 1) require_namespaces("emoa")

      # sampler
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(search_space_sampler)
      } else {
        assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
      }

      # r_min is the budget of a single configuration in the first stage
      # r_max is the maximum budget of a single configuration in the last stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the last stage (scaled)
      # R in the original paper
      r = r_max / r_min

      # s_max + 1 is the number of brackets and the number stages of the first bracket
      s_max = floor(log(r, eta))

      # approximately the used budget of an entire bracket
      # B in the original paper
      budget = (s_max + 1) * r

      # loop brackets
      for (s in s_max:0) {
        # number of configurations in the first stage
        n = ceiling((budget / r) * (eta^s) / (s + 1))
        # budget of a single configuration in the first stage
        rs = r * eta^(-s)

        # loop stages by calling successive halving subroutine
        successive_halving(s, rs, r_scale = r_min, n, eta, sampler, inst, bracket = s)
      }
    }
  )
)


