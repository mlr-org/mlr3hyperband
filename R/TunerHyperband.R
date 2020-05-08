#' @title Tuner using the Hyperband algorithm
#'
#' @aliases mlr_tuners_hyperband
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3tuning::Tuner].
#'
#' @description
#' Subclass for hyperband tuning.
#'
#' Hyperband is a budget oriented-procedure, weeding out suboptimal
#' performing configurations early in a sequential training process,
#' increasing tuning efficiency as a consequence.
#'
#' For this, several brackets are constructed with an associated set of configurations
#' for each bracket. Each bracket as several stages.
#'
#' Different brackets are initialized with different amounts of configurations
#' and different budget sizes. To get an idea of how the bracket
#' layout looks like for a given
#' argument set, please have a look in the `details`.
#' For more information about the algorithm, please check out the mlr3book. (ADD MISSING REFERENCE)
#'
#' To identify the budget for evaluating hyperband, the user has to specify
#' explicitly which hyperparameter of the learner influences the budget
#' by tagging a single hyperparameter in the [paradox::ParamSet] with `"budget"`.
#' An alternative approach using subsampling and pipelines is described below.
#'
#' Naturally, hyperband terminates once all of its brackets are evaluated,
#' so a [mlr3tuning::Terminator] in the tuning instance acts as an upper
#' bound and should be only set to a low value if one is unsure of how long
#' hyperband will take to finish under the given settings.
#'
#' @section Construction:
#' ```
#' TunerHyperband$new(eta = 2, sampler = NULL)
#' tnr("hyperband")
#' ```
#'
#' @section Parameters:
#' * `eta` :: `numeric(1)`\cr
#'   Fraction parameter of the successive halving algorithm: With every step
#'   the configuration budget is increased by a factor of `eta` and only the
#'   best `1/eta` configurations are used for the next stage. Non-integer
#'   values are supported, but `eta` is not allowed to be less or equal 1.
#'
#' * `sampler` :: [R6::R6Class] object inheriting from [paradox::Sampler]\cr
#'   Object defining how the samples of the parameter space should be drawn
#'   during the initialization of each bracket. The default is uniform sampling.
#'
#' @section Fields:
#' * `info` :: [data.table::data.table()]\cr
#'   Table containing information about the intermediate values, matching the
#'   the indices of the original reference instead of the console logs.
#'   It holds the following columns:
#'   * `bracket` :: `integer()`\cr
#'     The console logs about the bracket index are actually not matching
#'     with the original hyperband algorithm, which counts down the brackets
#'     and stops after evaluating bracket 0. The true bracket indices are
#'     given in this column.
#'   * `bracket_stage` :: `integer()`\cr
#'     The bracket stage of each bracket. Hyperband starts counting at 0.
#'   * `budget_scaled` :: `integer()`\cr
#'     The intermediate budget in each bracket stage calculated by hyperband.
#'     Because hyperband is originally only considered for budgets starting at 1, some
#'     rescaling is done to allow budgets starting at different values.
#'     For this, budgets are internally divided by the lower budget bound to
#'     get a lower budget of 1. Before the learner
#'     receives its budgets for evaluation, the budget is transformed back to
#'     match the original scale again.
#'   * `budget_real` :: `integer()`\cr
#'     The real budget values the learner uses for evaluation after hyperband
#'     calculated its scaled budget.
#'   * `n_configs` :: `integer()`\cr
#'     The amount of evaluated configurations in each stage. These correspond
#'     to the `r_i` in the original paper.
#'
#' @section Hyperband without learner budget:
#' Thanks to \CRANpkg{mlr3pipelines}, it is possible to use hyperband in combination
#' with learners lacking a natural budget parameter.
#' For example, any [mlr3::Learner] can be augmented with a
#' [PipeOp][mlr3pipelines::PipeOp] operator such as [PipeOpSubsample][mlr3pipelines::PipeOpSubsample].
#' With the subsampling rate as budget parameter, the resulting [GraphLearner][mlr3pipelines::GraphLearner]
#' is fitted on small proportions of the [Task][mlr3::Task] in the first brackets, and on the
#' complete Task in last brackets.
#' See examples for some code.
#'
#' @section Custom sampler:
#' Hyperband supports custom [Sampler][paradox::Sampler] object for initial
#' configurations in each bracket.
#' A custom sampler may look like this (the full example is given in the
#' `examples` section):
#' ```
#' # - beta distribution with alpha = 2 and beta = 5
#' # - categorical distribution with custom probabilities
#' sampler = SamplerJointIndep$new(list(
#'   Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
#'   Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
#' ))
#' ```
#'
#' @section Runtime scaling w.r.t. the chosen budget:
#' The calculation of each bracket currently assumes a linear runtime in the
#' chosen budget parameter is always given. Hyperband is designed so each
#' bracket requires approximately the same runtime as the sum of the budget
#' over all configurations in each bracket is roughly the same. This will not
#' hold true once the scaling in the budget parameter is not linear
#' anymore, even though the sum of the budgets in each bracket remains the
#' same. A basic example can be viewed by calling the function
#' `hyperband_brackets` below with the arguments `R = 2` and `eta = 2`. If we
#' run a learner with O(budget^2) time complexity, the runtime of the last
#' bracket will be 33% longer than the first bracket
#' (time of bracket 1 = 2 * 1^2 + 2^2 = 6; time of bracket 2 = 2 * 2^2 = 8).
#' Of course, this won't break anything, but it should be kept in mind when
#' applying hyperband. A possible adaption would be to introduce a trafo,
#' like it is shown in the `examples`.
#'
#' @details
#' This sections explains the calculation of the constants for each bracket.
#' A small overview will be given here, but for more details please check
#' out the original paper (see `references`).
#' To keep things uniform with the notation in the paper (and to safe space
#' in the formulas), `R` is used for the upper budget that last remaining configuration should reach.
#' The formula to calculate the amount of brackets is `floor(log(R, eta)) + 1`.
#' To calculate the starting budget in each bracket, use
#' `R * eta^(-s)`, where `s` is the maximum bracket minus the current bracket
#' index.
#' For the starting configurations in each bracket it is
#' `ceiling((B/R) * ((eta^s)/(s+1)))`, with `B = (bracket amount) * R`.
#' To receive a table with the full brackets layout, load the following function
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
#'   names(result) = c("bracket", "bracket_stage", "budget", "n_configs")
#'   return(result)
#' }
#'
#' hyperband_brackets(R = 81L, eta = 3L)
#' ```
#'
#' @section Logging:
#' When loading the [mlr3hyperband] package, three loggers based on the [lgr]
#' package are made available.
#' One is called `mlr3`, the other `mlr3/mlr3tuning` and the last
#' `mlr3/mlr3tuning/mlr3hyperband`. The first tow are the
#' original ones of [mlr3] and [mlr3tuning], while the latter was added
#' especially for [mlr3hyperband].
#' Each logger is responsible for loggings in executed code of each respective
#' package. This means [mlr3tuning] code executed in [mlr3hyperband] is logged
#' by `mlr3/mlr3tuning` and NOT `mlr3/mlr3tuning/mlr3hyperband`.
#' To change the behaviour of each logger, run
#' ```
#' # mlr3 add info logs
#' lgr::get_logger("mlr3")$set_threshold("info")
#' # mlr3tuning add info logs
#' lgr::get_logger("mlr3/mlr3tuning")$set_threshold("info")
#' # mlr3hyperband add info logs (already set by default)
#' lg$set_threshold("info")
#' ```
#' But be careful as this will add a lot of clutter to the logs.
#'
#' @references
#' \cite{mlr3hyperband}{li_2018}
#'
#' @examples
#' library(mlr3hyperband)
#' library(mlr3learners)
#' library(mlr3tuning)
#' library(mlr3)
#' library(paradox)
#'
#' set.seed(123)
#'
#' # define hyperparameter and budget parameter for tuning with hyperband
#' params = list(
#'   ParamInt$new("nrounds", lower = 1, upper = 16, tag = "budget"),
#'   ParamDbl$new("eta", lower = 0, upper = 1),
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
#' \dontrun{
#' tuner$tune(inst)
#'
#' # return the best evaluation
#' inst$best()
#'
#' # print all evaluations
#' print(inst$archive())
#' # print layout of the brackets
#' print(tuner$info)
#' }
#'
#'
#' ### use parameter trafo to convert budget parameter
#'
#' set.seed(123)
#'
#' # define hyperparameter and budget parameter for tuning with hyperband
#' ps = ParamSet$new(list(
#'   ParamInt$new("nrounds", lower = 1, upper = 10, tags = "budget"),
#'   # ParamDbl$new("eta",     lower = 0, upper = 1),
#'   ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
#' ))
#'
#' ps$trafo = function(x, param_set) {
#'   x$nrounds = round(log(x$nrounds)) + 1L
#'   return(x)
#' }
#'
#' inst = TuningInstance$new(
#'   tsk("iris"),
#'   lrn("classif.xgboost"),
#'   rsmp("holdout"),
#'   msr("classif.ce"),
#'   ps,
#'   term("evals", n_evals = 100000)
#' )
#'
#' # eta can be a double
#' tuner = TunerHyperband$new(eta = 1.9)
#' \dontrun{
#' tuner$tune(inst)
#'
#' # return the best evaluation
#' inst$best()
#'
#' # print all evaluations
#' print(inst$archive())
#' # print layout of the brackets
#' print(tuner$info)
#' }
#'
#' ### use subsampling for budget
#'
#' library(mlr3pipelines)
#' set.seed(123)
#' ll = po("subsample") %>>% lrn("classif.rpart")
#'
#' # define with extended hyperparameters with subsampling fraction as budget
#' # ==> no learner budget is required
#' params = list(
#'   ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
#'   ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10),
#'   ParamDbl$new("subsample.frac", lower = 0.1, upper = 1, tags = "budget")
#' )
#'
#' # define TuningInstance with the Graph Learner and the extended hyperparams
#' inst = TuningInstance$new(
#'   tsk("iris"),
#'   ll,
#'   rsmp("holdout"),
#'   msr("classif.ce"),
#'   ParamSet$new(params),
#'   term("evals", n_evals = 100000)
#' )
#'
#' # define and call hyperband as usual
#' tuner = TunerHyperband$new(eta = 4L)
#' \dontrun{
#' tuner$tune(inst)
#'
#' # return the best evaluation
#' inst$best()
#'
#' # print all evaluations
#' print(inst$archive())
#' # print layout of the brackets
#' print(tuner$info)
#' }
#' @export
TunerHyperband = R6Class("TunerHyperband",
  inherit = Tuner,

  public = list(

    # storing non-printed logging information
    info = NULL, #FIXME should all be in opt archive

    # create hyperband parameters and init super class (Tuner)
    initialize = function() {

      ps_hyperband = ParamSet$new(list(
        ParamDbl$new("eta", lower = 1.0001, tags = "required", default = 2),
      ))

      ps_hyperband$values = list(eta = 2)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = ps_hyperband,
        properties = c("dependencies")
      )
    }
  ),

  private = list(

    .optimize = function(inst) {

      # define aliases for better readability
      eta = self$param_set$values$eta
      rr = inst$resampling
      ps = inst$param_set
      task = inst$task
      msr_ids = ids(inst$measures)
      to_minimize = map_lgl(inst$measures, "minimize")

      # name of the hyperparameters with a budget tag
      budget_id = ps$ids(tags = "budget")
      # check if we have EXACTLY 1 budget parameter, or else throw an informative error
      if (length(budget_id) != 1) {
        stopf("Exactly one hyperparameter must be tagged with 'budget'")
      }

      # budget parameter MUST be defined as integer or double in paradox
      assert_choice(ps$class[[budget_id]], c("ParamInt", "ParamDbl"))
      ps_sampler = ps$subset(setdiff(ps$ids(), budget_id))
      sampler = SamplerUnif$new(ps_sampler)

      # use parameter tagged with 'budget' as budget for hyperband
      budget_lower = ps$lower[[budget_id]]
      budget_upper = ps$upper[[budget_id]]

      # we need the budget to start with a SMALL NONNEGATIVE value
      assert_number(budget_lower, lower = 1e-8)

      # rescale config max budget (:= 'R' in the original paper)
      # this represents the maximum budget a single configuration
      # will run for in the last stage of each bracket
      config_max_b = budget_upper / budget_lower

      # cannot use config_max_b due to stability reasons
      bracket_max = floor(log(budget_upper, eta) - log(budget_lower, eta))
      # <=> eta^bracket_max = config_max_b
      lg$log(
        "info",
        "Amount of brackets to be evaluated = %i, ",
        bracket_max + 1
      )

      # 'B' is approximately the used budget of an entire bracket.
      # The reference states a single execution of hyperband uses (smax+1) * B
      # amount of budget, and with (smax+1) as the amount of brackets follows
      # the claim. (smax is 'bracket_max' here)
      B = (bracket_max + 1L) * config_max_b

      # outer loop - iterating over brackets
      for (bracket in seq(bracket_max,0)) {

        # for less confusion of the user we start the print with bracket 1
        lg$log(
          "info",
          "Start evaluation of bracket %i",
          bracket_max - bracket + 1
        )

        # amount of active configs and budget in bracket
        mu_start = mu_current =
          ceiling((B * eta^bracket) / (config_max_b * (bracket + 1)))

        budget_start = budget_current =
          config_max_b / eta^bracket

        # generate design based on given parameter set and sampler
        active_configs = sampler$sample(mu_current)$data

        # inner loop - iterating over bracket stages
        for (stage in seq(0,bracket)) {

          # amount of configs of the previous stage
          mu_previous = mu_current

          # make configs smaller, increase budget and increment stage counter
          mu_current = floor(mu_start / eta^stage)
          budget_current = budget_start * eta^stage

          # rescale budget back to real world scale
          budget_current_real = budget_current * budget_lower
          # round if the budget is an integer parameter
          if (ps$class[budget_id] == "ParamInt") {
            budget_current_real = round(budget_current_real)
          }

          lg$log(
            "info",
            "Training %i configs with budget of %g for each",
            mu_current,
            budget_current_real
          )

          # only rank and pick configurations if we are not in the first stage
          if (stage > 0) {

            # get performance of each active configuration
            configs_perf = inst$bmr$score(inst$measures)
            n_rows       = nrow(configs_perf)
            configs_perf = configs_perf[(n_rows - mu_previous + 1):n_rows]

            # select best mu_current indices
            if (length(msr_ids) < 2) {

              # single crit
              ordered_perf = order(
                configs_perf[[msr_ids]],
                decreasing = !to_minimize
              )
              best_indices = ordered_perf[seq_len(mu_current)]

            } else {

              # multi crit
              best_indices = nds_selection(
                points = t(as.matrix(configs_perf[, msr_ids, with = FALSE])),
                n_select = mu_current,
                minimize = to_minimize
              )
            }

            # update active configurations
            assert_integer(
              best_indices, lower = 1, upper = nrow(active_configs)
            )
            active_configs = active_configs[best_indices]
          }

          # overwrite active configurations with the current budget
          active_configs[[budget_id]] = budget_current_real

          # extend active_configs with extras
          rbind(active_configs,
            bracket = bracket,
            bracket_stage = stage,
            budget_scaled = budget_current,
            budget_real = budget_current_real,
            n_configs = mu_current
          )

          # possible halt by terminator during evaluation
          # INFO logs in the following function call are ignored by default
          # set lgr::lgr$set_threshold(400) to include them
          inst$eval_batch(active_configs)

          )
        }
      }

      lg$log(
        "info",
        "Done."
        ### useful or just clutter?:
        # Total evaluations: %i with a total budget spend of %f.",
        # sum(self$info$n_configs),
        # sum(self$info$budget_real * self$info$n_configs)
      )
    }
  )
)
