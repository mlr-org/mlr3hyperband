#' @title TunerHyperband
#'
#' @aliases mlr_tuners_hyperband
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3tuning::Tuner].
#'
#' @description
#' Subclass for hyperband tuning.
#'
#' Hyperband is a budget oriented procedure putting more ressources on more 
#' promising configurations, increasing tuning efficiency as a consequence. 
#' For this, several brackets are constructed with different starting 
#' configurations in each. Each bracket has a different amount of stages 
#' with a different starting budget -- in general the more stages 
#' the lower the budget at first. Once a stage of a bracket is evaluated, the 
#' best `1/eta` configurations are kept, while the rest is discarded. The 
#' remaining configurations are then transfered to the next bracket stage, 
#' where training is continued with an increase of the budget by the factor of 
#' `eta`. This continuous iteratively for every bracket stage until the upper 
#' limit of the budget is reached. In the end, and aggregated over all brackets,
#' we have a lot of evaluated configurations with only a small handful being 
#' trained on the upper limit of the budget. This safes a lot of training time 
#' on configurations, that look unpromising on a low budget, as they are 
#' skipped for further evaluation. 
#' There are currently two ways to identify the
#' budget during tuning. One is by using the size of the training set as the 
#' budget, with the full set as the maximum budget (see the argument 
#' `use_subsamp`).
#' The other way is by explicitly specifying which learner's hyperparameter 
#' is the budget (see in the examples of how to do this in an 
#' [paradox::ParamSet] object).
#' Naturally, hyperband terminates once all of its brackets are evaluated,
#' so a terminator in tuning instance acts as an upper bound and should be 
#' only set to a low value if one is unsure how long hyperband will take to
#' finish under the given settings.
#'
#' @section Construction:
#' ```
#' TunerHyperband$new(eta = 3L)
#' tnr("hyperband")
#' ```
#'
#' @section Parameters:
#' This tuner currently supports the following hyperparameters:
#'
#' * `eta` :: `integer(1)` \cr
#'   Fraction parameter of the successive halving algorithm: With every step
#'   the configuration budget is increased by a factor of `eta` and only the
#'   best `1/eta` configurations are used for the next step.
#' * `use_supsamp` :: `bool(1)` \cr
#'   (Experimental feature)
#'   Should subsampling be used instead of a budget parameter of the learner?
#'   If true, the fraction of the subsample will be used as the budget in the
#'   hyperband algorithm with a lower budget of `0.1` and a maximum budget of
#'   `1.0`. Keep in mind the lower budget may never be used for any bracket,
#'   while the maximum budget is always the budget in the last stage of a
#'   bracket. This is influenced by `eta`: The formula for calculating the
#'   starting budget of each bracket is `maximum_budget / lower_budget * eta^(-bracket)`.
#' * `sampler` :: `[R6::R6Class] object inheriting from [paradox::Sampler]` \cr
#'   Object defining how the samples of the parameter space should be drawn
#'   during the initialization of each bracket. If no argument is given,
#'   uniform samples will be drawn in each hyperparameter dimension. Keep in
#'   mind either all parameters are handled in the [paradox::Sample] object
#'   or none. The budget parameter (if one is given) is an expection and will
#'   be ignored even if specified.
#'
#' @references \url{https://arxiv.org/abs/1603.06560}
#' @family mlr3tuning::Tuner
#' # see ?Tuner
#'
#' @examples
#' library(mlr3hyperband)
#' library(mlr3learners)
#' set.seed(123)
#'
#' # define hyperparameter and budget parameter for tuning with hyperband
#' params = list(
#'   ParamInt$new("nrounds", lower = 1, upper = 16, tag = "budget"),
#'   ParamDbl$new("eta",     lower = 0, upper = 1),
#'   ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
#' )
#'
#'
#' inst = TuningInstance$new(
#'   tsk("iris"),
#'   lrn("classif.xgboost"),
#'   rsmp("holdout"),
#'   msr("classif.ce"),
#'   ParamSet$new(params),
#'   term("evals", n_evals = 100000)
#' )
#'
#' # create custom sampler (optional):
#' # - beta distribution with alpha = 2 and beta = 5
#' # - categorical distribution with custom probabilities
#' sampler = SamplerJointIndep$new(list(
#'   Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
#'   Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
#' ))
#'
#' tuner = TunerHyperband$new(eta = 2L, sampler = sampler)
#' tuner$tune(inst)
#'
#' print(inst$archive())
#' print(tuner$info)
#'
#' @export
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
        lg$info(
          "Using train set fraction as budget with lower budget = %g and upper budget = %g",
          budget_lower,
          budget_upper
        )

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

      # rescale config max budget := 'R' in the original paper
      config_max_b = budget_upper/budget_lower
      eta          = self$param_set$values$eta

      # we add half machine eps for stability
      # try this floor(log(8.1 / 0.1)) = 3 (!!!). it should be 4!
      bracket_max = floor(log(config_max_b, eta) + 1e-8) 
      # eta^bracket_max = config_max_b
      lg$info(
        "Amount of brackets to be evaluated = %i, ", 
        bracket_max + 1
      )

      # outer loop - iterating over brackets
      for (bracket in bracket_max:0) {

        # for less confusion of the user we start the print with bracket 1
        lg$info("Start evaluation of bracket %i", bracket_max - bracket + 1)

        # initialize variables of the current bracket
        bracket_stage  = 1L
        B              = (bracket_max + 1L) * config_max_b
        # current nr of active configs in bracket
        mu_current     = ceiling((B * eta^bracket) / (config_max_b * (bracket + 1)))
        budget_current = config_max_b * eta^(-bracket)

        # generate design based on given parameter set and sampler
        design         = self$sampler$sample(mu_current)
        active_configs = design$data

        # inner loop - iterating over bracket stages
        for (stage in 0:bracket) {

          # rescale budget
          budget_current_real = budget_current * budget_lower

          lg$info(
            "Training %i configs with budget of %g for each",
            mu_current, 
            budget_current_real
          )

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

          # possible halt by terminator during evaluation
          # ignore logging in the next line - too much stuff flying around
          lgr::without_logging(
            instance$eval_batch(active_configs)
          )

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
          configs_perf   = instance$bmr$score(lapply(msr_ids, msr))

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
