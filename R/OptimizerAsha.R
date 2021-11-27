#' @title Optimizer Asynchronous Successive Halving
#'
#' @name mlr_optimizers_asha
#' @templateVar id asha
#'
#' @description
#' `OptimizerAsha` class that implements the asynchronous successive halving
#' algorithm (ASHA). ASHA parallelizes SHA ([OptimizerSuccessiveHalving]) by
#' promoting candidates to the next stage as soon as possible instead of waiting
#' for all candidates in the stage to finish. ASHA starts with sampling a
#' candidate point for each available worker. When an evaluation finishes and
#' the worker is available again, ASHA checks the stages from top to bottom for
#' promotable candidates. Promotions are possible when the evaluated candidates
#' belong to the top `1 / eta` of each stage. If no promotions are possible, a
#' new candidate is sampled and added to the base stage, which increases the
#' number of possible promotions for all stages.
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage, is set by
#' the lower bound of the budget parameter. The upper bound defines the maximum
#' budget (`r_max`) which which is allocated to the candidates in the last
#' stage.
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1/eta` points are promoted to the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn. The
#' default is uniform sampling.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional column that is specific
#' to the successive halving algorithm:
#'   * `stage` (`integer(1))`\cr
#'     Stage index. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#'
#' @section Parallelization:
#' The points are asynchronously evaluated with the \CRANpkg{future} package.
#' To select a parallel backend, use [future::plan()].
#'
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2020")`
#'
#' @export
#' @template example_optimizer
OptimizerAsha = R6Class("OptimizerAsha",
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
      minimize = ifelse(inst$archive$codomain$maximization_to_minimization == -1, TRUE, FALSE)
      n_workers = future::nbrOfWorkers()

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))
      integer_budget = search_space$class[[budget_id]] == "ParamInt"

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
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # s_max + 1 is the number of stages
      s_max = floor(log(r_max / r_min, eta))

      repeat({
        replicate(n_workers - inst$archive$n_in_progress, {
          xdt = get_job(s_max, eta, r_min, inst$archive, sampler, budget_id, integer_budget, minimize)
          inst$archive$add_evals(xdt, status = "proposed")
          inst$eval_proposed(async = TRUE, single_worker = FALSE)
        })
      inst$resolve_promise()
      })
     }
  )
)

#' @description
#' Returns promoted or sampled configuration.
#'
#' @param s_max (`integer(1)`)\cr
#'   Number of stages + 1
#' @param eta (`numeric(1)`)\cr
#'   Reduction factor.
#' @param r_min (`numeric(1)`)\cr
#'   Minimum budget in base stage.
#' @param archive ([ArchiveTuning]).
#' @param sampler ([paradox::Sampler]).
#' @param budget_id (`character(1)`)\cr
#'   Budget hyperparameter id.
#' @param integer_budget (`logical(1)`)\cr
#'   If `TRUE`, budget is rounded.
#' @param minimize (`logical()`)\cr
#'   If `TRUE`, measure is minimized.
#' @param brackets (`logical(1)`)\cr
#'   If `TRUE`, operates on brackets. `s_max` is bracket number.
#'
#' @noRd
get_job = function(s_max, eta, r_min, archive, sampler, budget_id, integer_budget, minimize, brackets = FALSE) {
  # s_max == 0 is random search with full budget
  if (nrow(archive$data) && s_max) {
    # try to promote configuration
    # iterate stages from top to base stage
    for (s in (s_max - 1):0) {
      data_stage = archive$data[get("stage") == s & get("status") == "evaluated"]
      # only use configurations of current bracket
      if (brackets) data_stage = data_stage[get("bracket") == s_max, ]
      if (!nrow(data_stage)) next

      # best n configurations in stage
      y = data_stage[, archive$cols_y, with = FALSE]
      n = floor(nrow(data_stage) / eta)
      row_ids = if (archive$codomain$length == 1) {
        head(order(unlist(y), decreasing = minimize), n)
      } else {
        nds_selection(points = t(as.matrix(y)), n_select = n, minimize = minimize)
      }
      candidates = data_stage[row_ids, ]

      # configurations in stage + 1
      promoted = archive$data[get("stage") == s + 1, ]
      if (brackets) promoted = promoted[get("bracket") == s_max]

      # select candidates that are not promoted yet
      promotable = setdiff(candidates$asha_id, promoted$asha_id)

      # promote configuration
      if (length(promotable)) {
        ri = r_min * eta^(s + 1)
        if (integer_budget) ri = as.integer(round(ri))
        xdt = candidates[get("asha_id") == promotable[1], c(archive$cols_x, "asha_id"), with = FALSE]
        set(xdt, j = budget_id, value = ri)
        set(xdt, j = "stage", value = s + 1L)
        return(xdt)
      }
    }
  }

  # if no promotion is possible, add new configuration to base stage
  xdt = sampler$sample(1)$data
  if (integer_budget) r_min = as.integer(round(r_min))
  set(xdt, j = budget_id, value = r_min)
  asha_id = if (!nrow(archive$data)) 1L else nrow(archive$data[get("stage") == 0]) + 1L
  set(xdt, j = "asha_id", value = asha_id)
  set(xdt, j = "stage", value = 0L)
  xdt
}
