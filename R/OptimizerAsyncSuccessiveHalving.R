#' @title Asynchronous Optimization via Random Search
#'
#' @include OptimizerAsync.R
#' @name mlr_optimizers_async_successive_halving
#'
#' @description
#' `OptimizerAsyncSuccessiveHalving` class that implements a simple Random Search.
#'
#' @templateVar id async_successive_halving
#' @template section_dictionary_optimizers
#'
#' @source
#' `r format_bib("bergstra_2012")`
#'
#' @export
OptimizerAsyncSuccessiveHalving = R6Class("OptimizerAsyncSuccessiveHalving",
  inherit = OptimizerAsync,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta                   = p_dbl(lower = 1.0001, default = 2),
        sampler               = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        adjust_minimum_budget = p_lgl(default = FALSE))

      param_set$values = list(eta = 2, sampler = NULL, adjust_minimum_budget = FALSE)

      super$initialize(
        id = "async_successive_halving",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit"),
        label = "Asynchronous Random Search",
        man = "bbotk::mlr_optimizers_successive_halving"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      n = pars$n
      eta = pars$eta
      sampler = pars$sampler
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")
      archive = inst$archive
      minimize = inst$archive$codomain$maximization_to_minimization == -1

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))

      # required for calculation of hypervolume
      if (inst$archive$codomain$length > 1) require_namespaces("emoa")

      # sampler
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(search_space_sampler)
      } else {
        assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
      }

      # top_n
      top_n = if (archive$codomain$length == 1) {
        function(y, n, minimize) {
          head(order(unlist(y), decreasing = minimize), n)
        }
      } else {
        function(y, n, minimize) {
          nds_selection(points = t(as.matrix(y)), n_select = n, minimize = minimize)
        }
      }

      # r_min is the budget of a single configuration in the first stage
      # r_max is the maximum budget of a single configuration in the last stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the last stage (scaled)
      r = r_max / r_min

      # number of stages if each configuration in the first stage uses the minimum budget
      # and each configuration in the last stage uses no more than the maximum budget
      s_max = floor(log(r, eta))

      # # reduce number of stages if n < r_max so that
      # # the last stages evaluates at least one configuration
      # sn = floor(log(n, eta))

      # s_max + 1 is the number of stages
      # s_max = min(sr, sn)

      # increase r_min so that the last stage uses the maximum budget
      if (pars$adjust_minimum_budget) r_min = r * eta^-s_max

      # usually the queue is empty but callbacks might have added points
      evaluate_queue_default(inst)

      while(!inst$is_terminated) {

        # sample new point
        sampler = SamplerUnif$new(search_space)
        xdt = sampler$sample(1)$data
        xss = transpose_list(xdt)
        xs = xss[[1]][inst$archive$cols_x]
        xs_trafoed = trafo_xs(xs, search_space)
        key = inst$archive$push_running_point(xs)

        # eval
        ys = inst$objective$eval(xs_trafoed)

        # push result
        inst$archive$push_result(key, ys = ys, x_domain = xs_trafoed, extra = list(stage = 0))

        for (s in seq(s_max)) {

          browser()

          current_stage = s -1

          data = archive$finished_data
          data_stage = data[list(current_stage), , on = "stage", nomatch = NULL]

          y = data_stage[[archive$cols_y]]

          promotable = max(floor(length(y) / eta), 1)
          y_sorted = sort(y, decreasing = !minimize)

          value = y_sorted[seq(promotable)]

          if (minimize) {
            
          }

          if (value < ys) {
            archive$push_running_point(xs, extra = list(stage = s + 1))
            ys = inst$objective$eval(xs_trafoed)
          } else {
            break
          }

          # c(1 , 2, 4)

          n = floor(nrow(data_stage) / eta)
          candidates = data_stage[top_n(y, n, minimize), ]

          ys > candidates[seq(promotable)]

          running_keys = archive$rush$running_tasks


        }

        stage = xdt[, budget_id, with = FALSE]

        y = data[, archive$cols_y, with = FALSE]
      }
    }
  )
)

#' @include aaa.R
optimizers[["async_successive_halving"]] = OptimizerAsyncSuccessiveHalving


if (FALSE) {
  flush_redis()
  library(rush)

  options(bbotk_local = TRUE)

  learner = lrn("classif.debug",
    x = to_tune(0),
    iter = to_tune(p_int(1, 16, tags = "budget")))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_successive_halving")
  tuner$optimize(instance)


}

