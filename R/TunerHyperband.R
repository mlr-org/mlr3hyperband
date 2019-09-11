#' @title TunerHyperband
#'
#' @aliases mlr_tuners_hyperband
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for hyperband tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per parameter,
#' see [paradox::generate_design_grid].
#' The points of the grid are evaluated in a random order.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Larger batches mean we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @section Construction:
#' ```
#' tuner = TunerHyperband$new(eta = 3L)
#' ```
#'
#' * `eta` :: `integer(1)` \cr
#'   Fraction parameter of the successive halving algorithm: With every step
#'   the configuration budget is increased by a factor of `eta` and only the
#'   best `1/eta` configurations are used for the next step.
#'
#' @references \url{https://arxiv.org/abs/1603.06560}
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerHyperband = R6Class(
  "TunerHyperband",
  inherit = Tuner,

  public = list(
    info = NULL,

    initialize = function(eta = 2) {#, config_max_budget = 1) {

      ps = ParamSet$new(list(
        ParamInt$new("eta", lower = 1L)#,
        #ParamInt$new("config_max_budget", lower = 0, upper = 100)
      ))

      ps$values = list(eta = eta)#, config_max_budget = config_max_budget)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = ps
      )
    }
  ),

  private = list(

    tune_internal = function(instance) {

      # define aliases for better readability
      rr = instance$resampling
      m  = instance$measures[[1L]]

      # bool vector of which parameters is a budget parameter
      budget_id = instance$param_set$ids(tags = "budget")
      assert_character(budget_id, len = 1L)

      # pick only the first budget param since multi budget is unsupported
      budget_lower = instance$param_set$lower[budget_id]
      budget_upper = instance$param_set$upper[budget_id]
      # we need the budget to start with a SMALL NONNEGATIVE value
      assert_number(budget_lower, lower = 1e-8)

      # rescale config max budget; note: cmb := R in the original paper
      cmb         = budget_upper/budget_lower
      eta         = self$param_set$values$eta
      # FIXME: we add half machine eps for stability
      # try this floor(log(8.1 / 0.1)) = 3 (!!!). it should be 4!
      bracket_max = floor(log(cmb, eta) + 1e-8) # eta^bracket_max = cmb
      messagef("cmb = %g, bracket_max = %i, ", cmb, bracket_max)
      # outer loop - iterating over brackets
      for (bracket in bracket_max:0) {

        # initialize variables of the current bracket
        bracket_stage  = 1L
        B              = (bracket_max + 1L) * cmb
        # current nr of active configs in bracket
        mu_current     = ceiling((B * eta^bracket) / (cmb * (bracket + 1)))
        budget_current = cmb * eta^(-bracket)

        # generate random design based on given parameter set
        ps             = instance$param_set
        design         = generate_design_random(ps, mu_current)
        active_configs = design$data

        # inner loop - iterating over bracket stages
        while(budget_current <= cmb && mu_current > 0L) {

          messagef(
            "Current budget = %g, mu = %i, ", budget_current, mu_current
          )

          # overwrite active configurations with the current budget
          #FIXME: this will break for multi budgets
          budget_current_real = budget_current * budget_lower
          active_configs[[budget_id]] = budget_current

          # evaluate active configurations
          # FIXME: vorsicht!!! hier können wir durch den terminator immer
          # rausfliegen

          instance$eval_batch(active_configs)

          # store information of current iteration with hash as primary key
          hash = tail(instance$bmr$hashes, mu_current)
          self$info = rbind(self$info, data.table(
            hash          = hash,
            bracket       = bracket,
            bracket_stage = bracket_stage,
            budget        = budget_current,
            budget_real   = budget_current_real,
            mu            = mu_current
          ))

          # make configs smaller, increase budget and increment stage counter
          mu_current     = floor(mu_current / eta)
          budget_current = budget_current * eta
          bracket_stage  = bracket_stage + 1L

          # get performance of each active configuration
          configs_perf   = instance$bmr$performance()
          # get the best ranked subset and reduce configs to it
          ordered_perf   = order(configs_perf[[m$id]], decreasing = !m$minimize)
          best_indeces   = ordered_perf[1:mu_current]
          active_configs = active_configs[best_indeces, ]
        }
      }
    }
  )
)
