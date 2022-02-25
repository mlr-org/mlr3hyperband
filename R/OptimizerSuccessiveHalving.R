#' @title Hyperparameter Optimization with Successive Halving
#'
#' @name mlr_optimizers_successive_halving
#' @templateVar id successive_halving
#'
#' @description
#' `OptimizerSuccessiveHalving` class that implements the successive halving
#' algorithm (SHA). SHA randomly samples `n` candidate points and
#' allocates a minimum budget (`r_min`) to all candidates. The candidates are
#' raced down in stages to a single best candidate by repeatedly increasing the
#' budget by a factor of `eta` and promoting only the best `1 / eta ` fraction
#' to the next stage. This means promising points are allocated a higher budget
#' overall and lower performing ones are discarded early on.
#'
#' #' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage, is set by
#' the lower bound of the budget parameter. The upper bound  defines the maximum
#' budget (`r_max`) which is allocated to the candidates in the last stage. The
#' number of stages is computed so that each candidate in base stage is
#' allocated the minimum budget and the candidates in the last stage are not
#' evaluated on more than the maximum budget. The following table is the stage
#' layout for `eta = 2`, `r_min = 1` and `r_max = 8`.
#'
#' | `i` | `n_i` | `r_i` |
#' | --: | ----: | ----: |
#' |   0 |     8 |     1 |
#' |   1 |     4 |     2 |
#' |   2 |     2 |     4 |
#' |   3 |     1 |     8 |
#'
#' `i` is stage number, `n_i` is the number of configurations and `r_i` is the
#' budget allocated to a single configuration.
#'
#' @section Parameters:
#' \describe{
#' \item{`n`}{`integer(1)`\cr
#' Number of points in base stage.}
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1 / eta` points are promoted to the next stage.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }
#' \item{`repetitions`}{`integer(1)`\cr
#' If `1` (default), optimization is stopped once all stages are evaluated.
#' Otherwise, optimization is stopped after `repetitions` runs of SHA. The
#' [bbotk::Terminator] might stop the optimization before all repetitions are
#' executed.
#' }
#' \item{`adjust_minimum_budget`}{`logical(1)`\cr
#' If `TRUE`, minimum budget is increased so that the last stage uses the
#' maximum budget defined in the search space.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional columns that are specific
#' to the successive halving algorithm:
#'   * `stage` (`integer(1))`\cr
#'     Stage index. Starts counting at 0.
#'   * `repetition` (`integer(1))`\cr
#'     Repetition index. Start counting at 1.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#' @template section_logging
#'
#' @source
#' `r format_bib("jamieson_2016")`
#'
#' @export
#' @template example_optimizer
OptimizerSuccessiveHalving = R6Class("OptimizerSuccessiveHalving",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        n           = p_int(lower = 1, default = 16),
        eta         = p_dbl(lower = 1.0001, default = 2),
        sampler     = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        repetitions = p_int(lower = 1L, default = 1, special_vals = list(Inf)),
        adjust_minimum_budget = p_lgl(default = FALSE)
      )
      param_set$values = list(n = 16L, eta = 2L, sampler = NULL, repetitions = 1, adjust_minimum_budget = FALSE)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = param_set,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = c("mlr3hyperband", "emoa"),
        man = "mlr3hyperband::mlr_optimizers_successive_halving"
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

      # increase r_min so that the last stage uses the maximum budget
      if (pars$adjust_minimum_budget) r_min = r * eta^-s_max

      repetition = 1
      while (repetition <= pars$repetitions) {
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
          set(xdt, j = "repetition", value = repetition)

          inst$eval_batch(xdt)
        }
        repetition = repetition + 1
      }
    }
  )
)
