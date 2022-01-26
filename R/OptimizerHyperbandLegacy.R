#' @title Optimizer using the Hyperband algorithm (Legacy Version)
#'
#' @name mlr_optimizers_hyperband_legacy
#'
#' @description
#' `OptimizerHyperband` class that implements hyperband optimization. Hyperband
#' (HB) repeatedly calls SH ([OptimizerSuccessiveHalving]) with different
#' numbers of starting points. A larger number of starting points corresponds to
#' a smaller budget allocated in the base stage. Each run of SH within HBX is
#' called a bracket. HBX considers `s_max + 1` brackets with `s_max =
#' floor(log(r_max / r_min, eta)`. The most explorative bracket `s = s_max`
#' constructs `s_max + 1` stages and allocates the minimum budget (`r_min`) in
#' the base stage. The minimum budget is increased in each bracket by a factor
#' of `eta` and the number of starting points is computed so that each bracket
#' approximately spends the same budget. Use [hyperband_schedule()] to get a
#' preview of the bracket layout.
#'
#' |   s |     |   3 |     |     |   2 |     |     |   1 |     |     |   0 |
#' | ---:| ---:| ---:| --- | ---:| ---:| --- | ---:| ---:| --- | ---:| ---:|
#' |   i |  ni |  ri |     |  ni |  ri |     |  ni |  ri |     |  ni |  ri |
#' |   0 |   8 |   1 |     |   6 |   2 |     |   4 |   4 |     |   8 |   4 |
#' |   1 |   4 |   2 |     |   3 |   4 |     |   2 |   8 |     |     |     |
#' |   2 |   2 |   4 |     |   1 |   8 |     |     |     |     |     |     |
#' |   3 |   1 |   8 |     |     |     |     |     |     |     |     |     |
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most
#' explorative bracket, is set by the lower bound of the budget parameter. The
#' upper bound defines the maximum budget (`r_max`) which which is allocated to
#' the candidates in the last stages.
#'
#' @templateVar id hyperband_legacy
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1 / eta` points are promoted to the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn in the
#' base stage of each bracket. The default is uniform sampling.
#' }
#' \item{`repeats`}{`logical(1)`\cr
#' If `FALSE` (default), hyperband terminates once all brackets are evaluated.
#' Otherwise, hyperband starts over again once the last bracket is evaluated.
#' }
#' \item{`repeats`}{`integer(1)`\cr
#' If `1` (default), optimization is stopped once all stages are evaluated.
#' Otherwise, optimization is stopped after `repeats` runs of SHA. The
#' [bbotk::Terminator] might stop the optimization before all repeats are
#' executed.
#' }}
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
#' @template section_progress_bars
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
        eta         = p_dbl(lower = 1.0001, tags = "required", default = 2),
        sampler     = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        repetitions = p_int(lower = 1L, default = 1),
        async       = p_lgl(default = FALSE)
      )
      param_set$values = list(eta = 2, sampler = NULL, repetitions = 1, async = FALSE)

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

      repetition = 1
      while (repetition <= pars$repetitions) {
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
              repetition = repetition,
              bracket = bracket,
              bracket_stage = stage,
              budget_scaled = budget_current,
              budget_real = budget_current_real,
              n_configs = mu_current
            )

            if (pars$async) {
              inst$archive$add_evals(xdt, status = "proposed")
              res = inst$eval_proposed(async = TRUE, single_worker = FALSE)
              future::resolve(res$promise, result = FALSE)
              inst$resolve_promise()
            } else {
              inst$eval_batch(xdt)
            }
          }
        }
        repetition = repetition + 1
      }
    }
  )
)
