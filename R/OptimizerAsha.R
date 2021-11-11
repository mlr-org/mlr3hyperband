#' @title Optimizer Asynchronous Successive Halving
#'
#' @name mlr_optimizers_asha
#'
#' @description
#' `OptimizerAsha` class that implements the asynchronous successive halving
#' algorithm.
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta` and only the
#' best `1/eta` points are promoted for the next stage. Non-integer values are
#' supported, but `eta` is not allowed to be less or equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }
#' \item{`s`}{`integer(1)`\cr
#' Minimum early-stopping rate.
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
#' @template section_logging
#'
#' @export
OptimizerAsha = R6Class("OptimizerAsha",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta     = p_dbl(lower = 1.0001, tags = "required", default = 2),
        sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        s       = p_int(lower = 0)
      )
      param_set$values = list(eta = 2, sampler = NULL, s = 0)

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
      s = pars$s
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")
      integer_budget = search_space$class[[budget_id]] == "ParamInt"
      minimize = !as.logical(inst$archive$codomain$maximization_to_minimization)
      n_workers = future::nbrOfWorkers()

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

      # r_min is the budget of a single configuration in the base stage
      # r_max is the maximum budget of a single configuration in the top stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the top stage (scaled)
      # R in the original paper
      r = r_max / r_min

      # k_max + 1 is the number of stages
      k_max = floor(log(r, eta))

      repeat({
        replicate(n_workers - inst$archive$n_in_progress, {
          xdt = get_job(k_max, eta, s, r_min, inst$archive, sampler, budget_id, integer_budget, minimize)
          inst$archive$add_evals(xdt, status = "proposed")
          inst$eval_proposed(async = TRUE, single_worker = FALSE)
        })
      inst$resolve_promise()
      })
     }
  )
)

get_job = function(k_max, eta, s, r_min, archive, sampler, budget_id, integer_budget, minimize) {
  if (nrow(archive$data)) {
    # try to promote configuration
    # iterate stages from top to base stage
    for (k in (k_max  - s - 1):0) {
      data_stage = archive$data[stage == k & status == "evaluated"]
      if (!nrow(data_stage)) next

      # select best n configurations of stage
      y = data_stage[, archive$cols_y, with = FALSE]
      n = floor(nrow(data_stage) / eta)
      row_ids = if (archive$codomain$length == 1) {
        head(order(unlist(y), decreasing = minimize), n)
      } else {
        nds_selection(points = t(as.matrix(y)), n_select = n, minimize = minimize)
      }
      candidates = data_stage[row_ids, ]

      # select candidates that are not promoted yet
      promotable = setdiff(candidates$asha_id, archive$data[stage == k + 1, asha_id])

      # promote configuration
      if (length(promotable)) {
        ri = r_min * eta^(k + s + 1)
        if (integer_budget) ri = as.integer(round(ri))
        xdt = candidates[asha_id == promotable[1], archive$cols_x, with = FALSE]
        set(xdt, j = budget_id, value = ri)
        asha_id = candidates[asha_id == promotable[1], asha_id]
        set(xdt, j = "stage", value = k + 1L)
        set(xdt, j = "asha_id", value = asha_id)
        return(xdt)
      }
    }
  }

  # if no promotion is possible, add new configuration to base stage
  xdt = sampler$sample(1)$data
  ri = r_min * eta^s
  if (integer_budget) ri = as.integer(round(ri))
  set(xdt, j = budget_id, value = ri)
  set(xdt, j = "stage", value = 0L)
  asha_id = if (!nrow(archive$data)) 1L else nrow(archive$data[stage == 0]) + 1L
  set(xdt, j = "asha_id", value = asha_id)
  xdt
}
