#' @title Optimizer using the Hyperband algorithm (Legacy Version)
#'
#' @name mlr_optimizers_hyperband_legacy
#'
#' @description
#' `OptimizerHyperbandLegacy` class that implements hyperband optimization. Hyperband
#' is a budget oriented-procedure, weeding out suboptimal performing
#' configurations early in a sequential training process, increasing
#' optimization efficiency as a consequence.
#'
#' For this, several brackets are constructed with an associated set of
#' configurations for each bracket. Each bracket as several stages. Different
#' brackets are initialized with different amounts of configurations and
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
#' @templateVar id hyperband_legacy
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
#' The [bbotk::Archive] holds the following additional columns that
#' are specific to the hyperband tuner:
#'   * `bracket` (`integer(1)`)\cr
#'     The console logs about the bracket index are actually not matching with
#'     the original hyperband algorithm, which counts down the brackets and
#'     stops after evaluating bracket 0. The true bracket indices are given in
#'     this column.
#'   * `bracket_stage` (`integer(1))`\cr
#'     The bracket stage of each bracket. Hyperband starts counting at 0.
#'   * `budget_scaled` (`numeric(1)`)\cr
#'     The intermediate budget in each bracket stage calculated by hyperband.
#'     Because hyperband is originally only considered for budgets starting at
#'     1, some rescaling is done to allow budgets starting at different values.
#'     For this, budgets are internally divided by the lower budget bound to get
#'     a lower budget of 1. Before the objective function receives its budgets
#'     for evaluation, the budget is transformed back to match the original
#'     scale again.
#'   * `budget_real` (`numeric(1)`)\cr
#'     The real budget values the objective function uses for evaluation after
#'     hyperband calculated its scaled budget.
#'   * `n_configs` (`integer(1)`)\cr
#'     The amount of evaluated configurations in each stage. These correspond to
#'     the `r_i` in the original paper.
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
#' @noRd
OptimizerHyperbandLegacy = R6Class("OptimizerHyperbandLegacy",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta = p_dbl(lower = 1.0001, tags = "required", default = 2),
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
      eta = self$param_set$values$eta
      sampler = self$param_set$values$sampler
      search_space = inst$search_space
      archive = inst$archive

      if (archive$codomain$length > 1) require_namespaces("emoa")

      # name of the hyperparameters with a budget tag
      budget_id = search_space$ids(tags = "budget")

      # check if we have EXACTLY 1 budget parameter, or else throw an informative error
      if (length(budget_id) != 1) stopf("Exactly one hyperparameter must be tagged with 'budget'")

      # budget parameter MUST be defined as integer or double in paradox
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))

      # construct unif sampler if non is given
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(search_space_sampler)
      } else {
        assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
      }

      # use parameter tagged with 'budget' as budget for hyperband
      budget_lower = search_space$lower[[budget_id]]
      budget_upper = search_space$upper[[budget_id]]

      # we need the budget to start with a SMALL NONNEGATIVE value
      assert_number(budget_lower, lower = 1e-8)

      # rescale config max budget (:= 'R' in the original paper)
      # this represents the maximum budget a single configuration
      # will run for in the last stage of each bracket
      config_max_b = budget_upper / budget_lower

      # cannot use config_max_b due to stability reasons
      bracket_max = floor(log(budget_upper, eta) - log(budget_lower, eta))
      # <=> eta^bracket_max = config_max_b
      lg$info("Amount of brackets to be evaluated = %i, ", bracket_max + 1)

      # 'B' is approximately the used budget of an entire bracket.
      # The reference states a single execution of hyperband uses (smax+1) * B
      # amount of budget, and with (smax+1) as the amount of brackets follows
      # the claim. (smax is 'bracket_max' here)
      B = (bracket_max + 1L) * config_max_b

      # outer loop - iterating over brackets
      for (bracket in seq(bracket_max, 0)) {

        # for less confusion of the user we start the print with bracket 1
        lg$info("Start evaluation of bracket %i", bracket_max - bracket + 1)

        # amount of active configs and budget in bracket
        mu_start = mu_current = ceiling((B * eta^bracket) / (config_max_b * (bracket + 1)))

        budget_start = budget_current = config_max_b / eta^bracket

        # generate design based on given parameter set and sampler
        active_configs = sampler$sample(mu_current)$data

        # inner loop - iterating over bracket stages
        for (stage in seq(0, bracket)) {

          # amount of configs of the previous stage
          mu_previous = mu_current

          # make configs smaller, increase budget and increment stage counter
          mu_current = floor(mu_start / eta^stage)
          budget_current = budget_start * eta^stage

          # rescale budget back to real world scale
          budget_current_real = budget_current * budget_lower
          # round if the budget is an integer parameter
          if (search_space$class[[budget_id]] == "ParamInt") {
            budget_current_real = round(budget_current_real)
          }

          lg$info("Training %i configs with budget of %g for each",
            mu_current, budget_current_real)

          # only rank and pick configurations if we are not in the first stage
          if (stage > 0) {

            # get performance of each active configuration
            data = archive$data[batch_nr %in% archive$n_batch]
            y = data[, archive$cols_y, with = FALSE]

            active_configs = if (archive$codomain$length == 1) {
              # single-crit
              archive$best(batch = archive$n_batch, n_select = mu_current)
            } else {
              # multi-crit
              archive$nds_selection(batch = archive$n_batch, n_select = mu_current)
            }
            active_configs = active_configs[, archive$cols_x, with = FALSE]
          }

          # overwrite active configurations with the current budget
          active_configs[[budget_id]] = budget_current_real

          # extend active_configs with extras
          xdt = cbind(active_configs,
            bracket = bracket,
            bracket_stage = stage,
            budget_scaled = budget_current,
            budget_real = budget_current_real,
            n_configs = mu_current
          )

          inst$eval_batch(xdt)
        }
      }
    }
  )
)
