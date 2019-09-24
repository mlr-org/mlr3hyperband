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
    sampler = NULL,
    use_subsampling = NULL,

    initialize = function(eta = 2, use_subsamp = FALSE, sampler = NULL) {

      ps_hyperband = ParamSet$new(list(
        ParamInt$new("eta", lower = 1L)
      ))

      ps_hyperband$values = list(eta = eta)
    
      # TODO: missing asserts
      self$sampler = sampler
      self$use_subsampling = use_subsamp

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = ps_hyperband,
        properties = c("dependencies")
      )
    }
  ),

  private = list(

    tune_internal = function(instance) {

      # define aliases for better readability
      rr = instance$resampling
      ps = instance$param_set
      task = instance$task
      to_minimize  = map_lgl(instance$measures, "minimize")
      msr_ids = ids(instance$measures)

      # construct unif sampler if non is given
      if (is.null(self$sampler)) {
        self$sampler = SamplerUnif$new(ps)
      }

      # use subsampling fraction as budget
      if (self$use_subsampling) {
        budget_lower = 0.1
        budget_upper = 1.0

      } else {
        # bool vector of which parameters is a budget parameter
        budget_id = instance$param_set$ids(tags = "budget")
        assert_character(budget_id, len = 1L)

        # use budget parameter as budget
        budget_lower = instance$param_set$lower[budget_id]
        budget_upper = instance$param_set$upper[budget_id]
      }

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

        # generate design based on given parameter set and sampler
        design         = self$sampler$sample(mu_current)
        active_configs = design$data

        # inner loop - iterating over bracket stages
        for (stage in 0:bracket) {

          messagef(
            "Current budget = %g, mu = %i, ", budget_current, mu_current
          )

          # rescale budget
          budget_current_real = budget_current * budget_lower

          # use subset of task according to budget
          if (self$use_subsampling) {

            po = mlr_pipeops$get(
              "subsample", 
              param_vals = list(frac = budget_current_real)
            )
            instance$task = po$train(list(instance$task))$output

          } else {
            # overwrite active configurations with the current budget
            active_configs[[budget_id]] = budget_current_real
          }

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
          configs_perf   = instance$bmr$score(msr_ids)

          # only rank and pick configurations if we are not in the last stage
          if (stage != bracket) {

            # select best mu_current indeces
            if (length(msr_ids) < 2) {

              # single crit
              ordered_perf = order(
                configs_perf[[msr_ids]],
                decreasing = !to_minimize
              )
              best_indeces = ordered_perf[1:mu_current]

            } else {

              # multi crit
              best_indeces = nds_selection(
                points = t(as.matrix(configs_perf[, msr_ids, with = FALSE])),
                n_select = mu_current,
                minimize = to_minimize
              )
            }

            # update active configurations
            active_configs = active_configs[best_indeces, ]
          }
        }
      }
    }
  )
)
