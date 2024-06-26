#' @title Optimizer Using the Hyperband Algorithm

#' @include OptimizerBatchSuccessiveHalving.R
#' @name mlr_optimizers_hyperband
#' @templateVar id hyperband
#'
#' @description
#' Optimizer using the Hyperband (HB) algorithm.
#' HB runs the [Successive Halving Algorithm][OptimizerBatchSuccessiveHalving] (SHA) with different numbers of stating configurations.
#' The algorithm is initialized with the same parameters as Successive Halving but without `n`.
#' Each run of Successive Halving is called a bracket and starts with a different budget `r_0`.
#' A smaller starting budget means that more configurations can be tried out.
#' The most explorative bracket allocated the minimum budget `r_min`.
#' The next bracket increases the starting budget by a factor of `eta`.
#' In each bracket, the starting budget increases further until the last bracket `s = 0` essentially performs a random search with the full budget `r_max`.
#' The number of brackets `s_max + 1` is calculated with `s_max = log(r_min / r_max)(eta)`.
#' Under the condition that `r_0` increases by `eta` with each bracket, `r_min` sometimes has to be adjusted slightly in order not to use more than `r_max` resources in the last bracket.
#' The number of configurations in the base stages is calculated so that each bracket uses approximately the same amount of budget.
#' The following table shows a full run of HB with `eta = 2`, `r_min = 1` and `r_max = 8`.
#'
#' | `s` |     |       |     3 |     |       |     2 |     |       |     1 |     |       |     0 |
#' | --: | --- | ----: | ----: | --- | ----: | ----: | --- | ----: | ----: | --- | ----: | ----: |
#' | `i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |     | `n_i` | `r_i` |
#' |   0 |     |     8 |     1 |     |     6 |     2 |     |     4 |     4 |     |     8 |     4 |
#' |   1 |     |     4 |     2 |     |     3 |     4 |     |     2 |     8 |     |       |       |
#' |   2 |     |     2 |     4 |     |     1 |     8 |     |       |       |     |       |       |
#' |   3 |     |     1 |     8 |     |       |       |     |       |       |     |       |       |
#'
#' `s` is the bracket number, `i` is the stage number, `n_i` is the number of configurations and `r_i` is the budget allocated to a single configuration.
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most explorative bracket, is set by the lower bound of the budget parameter.
#' The upper bound defines the maximum budget (`r_max`) which is allocated to the candidates in the last stages.
#'
#' @section Resources:
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#'  * [Tune](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/) the hyperparameters of XGBoost with Hyperband.
#'  * Use data [subsampling](https://mlr-org.com/gallery/series/2023-01-16-hyperband-subsampling/) and Hyperband to optimize a support vector machine.
#'
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#'   With every stage, the budget is increased by a factor of `eta` and only the best `1 / eta` points are promoted to the next stage.
#'   Non-integer values are supported, but `eta` is not allowed to be less or equal to 1.}
#' \item{`sampler`}{[paradox::Sampler]\cr
#'   Object defining how the samples of the parameter space should be drawn in the base stage of each bracket.
#'   The default is uniform sampling.}
#' \item{`repetitions`}{`integer(1)`\cr
#'   If `1` (default), optimization is stopped once all brackets are evaluated.
#'   Otherwise, optimization is stopped after `repetitions` runs of HB.
#'   The [bbotk::Terminator] might stop the optimization before all repetitions are executed.}
#' }
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional columns that are specific to HB:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'   * `repetition` (`integer(1))`\cr
#'     Repetition index. Start counting at 1.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#' @template section_logging
#'
#' @source
#' `r format_bib("li_2018")`
#'
#' @export
#' @template example_optimizer
OptimizerBatchHyperband = R6Class("OptimizerBatchHyperband",
  inherit = OptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta     = p_dbl(lower = 1.0001, tags = "required"),
        sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        repetitions = p_int(lower = 1L, default = 1, special_vals = list(Inf))
      )
      param_set$values = list(eta = 2, sampler = NULL, repetitions = 1)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = param_set,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = "mlr3hyperband",
        label = "Hyperband",
        man = "mlr3hyperband::mlr_optimizers_hyperband"
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
      # r_max is the maximum budget of a single configuration in the last stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the last stage (scaled)
      # R in the original paper
      r = r_max / r_min

      # s_max + 1 is the number of brackets and the number stages of the first bracket
      s_max = floor(log(r, eta))

      # approximately the used budget of an entire bracket
      # B in the original paper
      budget = (s_max + 1) * r

      # number of configurations in first stages
      n = ceiling((budget / r) * (eta^(0:s_max)) / ((0:s_max) + 1))

      repetition = 1
      while (repetition <= pars$repetitions) {
        # original hyperband algorithm iterates over brackets
        # this implementation iterates over stages with same budget
        # the number of iterations (s_max + 1) remains the same in both implementations
        for (s in s_max:0) {
          # budget of a single configuration in the first stage (unscaled)
          rs = r_min * r * eta^(-s)
          # sample initial configurations of bracket
          xdt = sampler$sample(n[s + 1])$data
          set(xdt, j = budget_id, value = rs)
          set(xdt, j = "stage", value = 0)
          set(xdt, j = "bracket", value = s)
          set(xdt, j = "repetition", value = repetition)

          # promote configurations of previous batch
          if (s != s_max) {
            archive = inst$archive
            data = archive$data[list(archive$n_batch), , on = "batch_nr"]
            minimize = ifelse(archive$codomain$maximization_to_minimization == -1, TRUE, FALSE)

            # for each bracket, promote configurations of previous stage
            xdt_promoted = map_dtr(s_max:(s + 1), function(i) {

              # number of configuration to promote
              ni = floor(n[i + 1] * eta^(-(i - s)))

              # get performances of previous stage
              data_bracket = data[get("bracket") == i, ]
              y = data_bracket[, archive$cols_y, with = FALSE]

              # select best ni configurations
              row_ids = if (archive$codomain$length == 1) {
                head(order(unlist(y), decreasing = minimize), ni)
              } else {
                nds_selection(points = t(as.matrix(y)), n_select = ni, minimize = minimize)
              }
              data_bracket[row_ids, ]
            })

            # increase budget and stage
            xdt_promoted = xdt_promoted[, c(inst$archive$cols_x, "stage", "bracket", "repetition"), with = FALSE]
            set(xdt_promoted, j = budget_id, value = rs)
            set(xdt_promoted, j = "stage", value = xdt_promoted[["stage"]] + 1)

            xdt = rbindlist(list(xdt, xdt_promoted), use.names = TRUE)
          }

          if (search_space$class[[budget_id]] == "ParamInt") set(xdt, j = budget_id, value = round(xdt[[budget_id]]))
          inst$eval_batch(xdt)
        }
        repetition = repetition + 1
      }
    }
  )
)

#' @include aaa.R
optimizers[["hyperband"]] = OptimizerBatchHyperband
