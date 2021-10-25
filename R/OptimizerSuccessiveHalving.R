#' @title Hyperparameter Optimization with Successive Halving
#'
#' @name mlr_optimizers_successive_halving
#'
#' @description
#' `OptimizerSuccessiveHalving` class that implements the successive halving
#' algorithm. The algorithm samples `n` points and evaluates them with the
#' smallest budget (lower bound of the `budget` parameter). With every stage the
#' budget is increased by a factor of `eta` and only the best `1/eta` points are
#' promoted to the next stage. The optimization terminates when the maximum
#' budget is reached (upper bound of the `budget` parameter).
#'
#' To identify the budget, the user has to specify explicitly which parameter of
#' the objective function influences the budget by tagging a single parameter in
#' the search space ([paradox::ParamSet]) with `"budget"`.
#'
#' @section Parameters:
#' \describe{
#' \item{`n`}{`integer(1)`\cr
#' Number of points in first stage.}
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the point budget is increased by a factor of `eta`
#' and only the best `1/eta` points are used for the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn during
#' the initialization of each bracket. The default is uniform sampling.
#' }
#' \item{`repeats`}{`logical(1)`\cr
#' If `FALSE` (default), successive halving terminates once all stages are
#' evaluated. Otherwise, successive halving starts over again once the last
#' stage is evaluated.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional column that is specific
#' to the successive halving algorithm:
#'   * `stage` (`integer(1))`\cr
#'     Stage index. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_runtime
#' @template section_progress_bars
#' @template section_parallelization
#' @template section_logging
#'
#' @source
#' `r format_bib("jamieson_2016")`
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
#' optimizer = opt("successive_halving")
#'
#' # modifies the instance by reference
#' optimizer$optimize(instance)
#'
#' # best scoring evaluation
#' instance$result
#'
#' # all evaluations
#' as.data.table(instance$archive)
OptimizerSuccessiveHalving = R6Class("OptimizerSuccessiveHalving",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        n       = p_int(lower = 1, default = 16),
        eta     = p_dbl(lower = 1.0001, default = 2),
        sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        repeats = p_lgl(default = FALSE)
      )
      param_set$values = list(n = 16L, eta = 2L, sampler = NULL, repeats = FALSE)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = param_set,
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
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))

      # required for calculation of hypervolume
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
      r = r_max / r_min

      # number of stages if each configuration in the first stage uses the minimum budget
      # and each configuration in the last stage uses no more than maximum budget
      sr = floor(log(r, eta))

      # reduce number of stages if n < r_max so that
      # the last stages evaluates at least one configuration
      sn = floor(log(n, eta))

      # s_max + 1 is the number of stages
      s_max = min(sr, sn)

      repeat({
        # iterate stages
        for (i in 0:s_max) {
          # number of configurations in stage
          ni = floor(n * eta^(-i))
          # budget of a single configuration in stage
          ri = r_min * eta^i

          if (search_space$class[[budget_id]] == "ParamInt") ri = round(ri)

          if (i == 0) {
            xdt = sampler$sample(ni)$data
          } else {
            # get performances of previous stage
            archive = inst$archive

            xdt = if (archive$codomain$length == 1) {
              archive$best(batch = archive$n_batch, n_select = ni)
            } else {
              archive$nds_selection(batch = archive$n_batch, n_select = ni)
            }
            xdt = xdt[, archive$cols_x, with = FALSE]
          }
          # increase budget and stage
          set(xdt, j = budget_id, value = ri)
          set(xdt, j = "stage", value = i)

          inst$eval_batch(xdt)
        }
        if (!pars$repeats) break
      })
    }
  )
)
