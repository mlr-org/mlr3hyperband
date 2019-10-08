#' @title Tuner using the Hyperband algorithm
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
#'   starting budget of each bracket is 
#'   `maximum_budget / lower_budget * eta^(-bracket)`.
#' * `sampler` :: `[R6::R6Class] object inheriting from [paradox::Sampler]` \cr
#'   Object defining how the samples of the parameter space should be drawn
#'   during the initialization of each bracket. If no argument is given,
#'   uniform samples will be drawn in each hyperparameter dimension. Keep in
#'   mind either all parameters are handled in the [paradox::Sample] object
#'   or none. The budget parameter (if one is given) is an expection and will
#'   be ignored even if specified.
#'
#'
#' @section Return:
#' Using the `tune()` method of `mlr3tuning` does not return anything, but the
#' results can be easily extracted from the (during tuning) modified 
#' `[mlr3tuning::TuningInstance]` object. To return the best evaluation so far, simply
#' use the method `best()` on the tuned instance. If all the evaluations are
#' desired to be explored, use the method `archive()` instead.
#'
#' @section Runtime scaling w.r.t. the chosen budget:
#' The calculation of each bracket currently assumes a linear runtime in the
#' chosen budget parameter is always given. Hyperband is designed so each
#' bracket requires approximately the same runtime as the sum of the budget
#' over all configurations in each bracket is roughly the same. This won't
#' hold true once the scaling in the budget parameter isn't linear
#' anymore, even thought the sum of the budgets in each bracket remain the 
#' same. A basic example can be viewed by calling the function
#' `hyperband_brackets` below with the arguments `R = 2` and `eta = 2`. If we
#' run a learner with O(budget^2) time complexity, the runtime of the last
#' bracket will be 33% longer than the first bracket 
#' (time of bracket 1 = 2 * 1^2 + 2^2 = 6; time of bracket 2 = 2 * 2^2 = 8).
#' Of course, this won't break anything, but it should be kept in mind when
#' applying Hyperband.
#'
#' @details The calculations of the brackets layout is quite unintuitive.
#' A small overview will be given here, but for more details please check
#' out the original paper (see `references`).
#' To keep things uniform with the notation in the paper (and to safe space
#' in the formulas) `R` is used for the upper budget.
#' The formula to calculate the bracket amount is `floor(log(R, eta)) + 1`.
#' To calculate the starting budget in each bracket use
#' `R * eta^(-s)`, where `s` is the maximum bracket minus the current bracket
#' index.
#' For the starting configurations in each bracket it's
#' `ceiling((B/R) * ((eta^s)/(s+1)))`, with `B = (bracket amount) * R`.
#' To receive a table with the full brackets layout load the following function
#' and execute it for the desired `R` and `eta`.
#'
#' ```
#' hyperband_brackets = function(R, eta) {
#'
#'   result = data.frame()
#'   smax = floor(log(R, eta))
#'   B = (smax + 1) * R
#'
#'   # outer loop - iterate over brackets
#'   for (s in smax:0) {
#'
#'     n = ceiling((B/R) * ((eta^s)/(s+1)))
#'     r = R * eta^(-s)
#'
#'     # inner loop - iterate over bracket stages
#'     for (i in 0:s) {
#'
#'       ni = floor(n * eta^(-i))
#'       ri = r * eta^i
#'       result = rbind(result, c(smax - s + 1, i + 1, ri, ni))
#'     }
#'   }
#'
#'   names(result) = c("bracket", "bracket_stage", "budget", "n.configs")
#'   return(result)
#' }
#'
#' hyperband_brackets(R = 81L, eta = 3L)
#' ```
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
#' # return the best evaluation
#' inst$best()
#'
#' # print all evaluations
#' print(inst$archive())
#' # print layout of the brackets
#' print(tuner$info)
#'
#' @export
TunerHyperband = R6Class(
  "TunerHyperband",
  inherit = Tuner,

  public = list(
    # storing non-printed logging information
    info = NULL,
    # Object defining sampling of each learner hyperparameter
    sampler = NULL,
    # Bool: if we are using subsampling or the learner as budget
    use_subsampling = NULL,

    # create hyperband parameters and init super class (Tuner)
    initialize = function(eta = 2L, use_subsamp = FALSE, sampler = NULL) {

      # check input for correctness
      assert_int(eta, lower = 2)
      assert_logical(use_subsamp)
      assert_r6(sampler, classes = "Sampler", null.ok = TRUE)


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
