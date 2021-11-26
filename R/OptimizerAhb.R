#' @title Optimizer Asynchronous Hyperband
#'
#' @name mlr_optimizers_ahb
#' @templateVar id ahb
#'
#' @description
#' `OptimizerAhb` class that implements the asynchronous hyperband algorithm.
#' Asynchronous hyperband (AHB) repeatedly runs ASHA ([OptimizerAsha]) with
#' different minimum budgets in the base stage. Each run of ASHA within AHB is
#' called a bracket. AHB considers `s_max + 1` brackets with
#' `s_max = floor(log(r_max / r_min, eta)`. The most explorative bracket
#' `s = s_max` constructs `s_max + 1` stages and allocates the minimum budget
#' (`r_min`) in the base stage.  The minimum budget (`r_min`) is increased in
#' each bracket by a factor of `eta` until the maximum budget is allocated in
#' the base stage. The bracket `s = 0` is a random search with full budget. Each
#' ASHA run uses `1 / s_max + 1` of the [bbotk::Terminator].
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most
#' explorative bracket, is set by the lower bound of the budget parameter. The
#' upper bound defines the maximum budget (`r_max`) which which is allocated to
#' the candidates in the last stages.
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
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional columns that are specific
#' to the hyperband algorithm:
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2020")`
#'
#' @export
#' @template example_optimizer
OptimizerAhb = R6Class("OptimizerAhb",
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
        packages = "mlr3hyperband"
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
      archive = inst$archive
      minimize = ifelse(archive$codomain$maximization_to_minimization == -1, TRUE, FALSE)
      n_workers = future::nbrOfWorkers()
      terminator = inst$terminator

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))
      integer_budget = search_space$class[[budget_id]] == "ParamInt"

      if (archive$codomain$length > 1) require_namespaces("emoa")

      # sampler
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(search_space_sampler)
      } else {
        assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
      }

      # check terminator
      if (!inherits(terminator, c("TerminatorClockTime", "TerminatorEvals", "TerminatorRunTime"))) {
        stopf("%s does not support %s. Use <TerminatorClockTime>, <TerminatorRunTime> or <TerminatorEvals>.", format(self), format(terminator))
      }

      # r_min is the budget of a single configuration in the base stage of the first bracket
      # r_max is the maximum budget of a single configuration in the last stage
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # s_max + 1 is the number of brackets
      s_max = floor(log(r_max / r_min, eta))

      for (s in s_max:0) {
        # rs_min is the budget of a single configuration in the base stage
        rs_min = r_min * eta^(s_max - s)
        repeat({
          replicate(n_workers - archive$n_in_progress, {
            xdt = get_job(s, eta, rs_min, archive, sampler, budget_id, integer_budget, minimize,
              brackets = TRUE)
            set(xdt, j = "bracket", value = s)
            archive$add_evals(xdt, status = "proposed")
            inst$eval_proposed(async = TRUE, single_worker = FALSE)
          })
        inst$resolve_promise()
        status = terminator$status(archive)
        if (status["current_steps"] / status["max_steps"] > 1 - s / (s_max + 1)) break
        })
      }
    }
  )
)
