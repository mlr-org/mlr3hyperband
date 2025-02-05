#' @export
OptimizerAsyncSuccessiveHalving2 = R6Class("OptimizerAsyncSuccessiveHalving2",
  inherit = OptimizerAsync,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta     = p_dbl(lower = 1.0001, default = 2),
        sampler = p_uty(custom_check = crate({function(x) check_r6(x, "Sampler", null.ok = TRUE)})))

      param_set$values = list(eta = 2, sampler = NULL)

      super$initialize(
        id = "async_successive_halving",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit", "async"),
        packages = "rush",
        label = "Asynchronous Successive Halving",
        man = "bbotk::mlr_optimizers_async_successive_halving"
      )
    },

    #' @description
    #' Performs the optimization on a [OptimInstanceAsyncSingleCrit] or [OptimInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsync].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([OptimInstanceAsyncSingleCrit] | [OptimInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      pars = self$param_set$values
      eta = pars$eta
      sampler = pars$sampler
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))

      # required for calculation of hypervolume
      if (inst$archive$codomain$length > 1) require_namespaces("emoa")

      # sampler
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))
      private$.sampler = if (is.null(sampler)) {
        SamplerUnif$new(search_space_sampler)
      } else {
        assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
        sampler
      }

      # function to select the n best configurations
      private$.top_n = if (inst$archive$codomain$length == 1) {
        function(data, cols_y, n, direction) {
          setorderv(data, cols = cols_y, order = direction)
          head(data, n)
        }
      } else {
        function(data, cols_y, n, direction) {
          points = t(as.matrix(data[, cols_y, with = FALSE]))
          ii = nds_selection(points, n, minimize = direction == 1)
          data[ii]
        }
      }

      # r_min is the budget of a single configuration in the first stage
      # r_max is the maximum budget of a single configuration in the last stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      private$.r_min = r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the last stage (scaled)
      r = r_max / r_min

      # number of stages if each configuration in the first stage uses the minimum budget
      # and each configuration in the last stage uses no more than maximum budget
      private$.s_max = floor(log(r, eta))

      optimize_async_default(inst, self)
    }
  ),

  private = list(
    .r_min = NULL,
    .s_max = NULL,
    .top_n = NULL,
    .sampler = NULL,

    .optimize = function(inst) {
      archive = inst$archive
      r_min = private$.r_min
      s_max = private$.s_max
      eta = self$param_set$values$eta
      budget_id = inst$search_space$ids(tags = "budget")
      direction = inst$archive$codomain$direction
      r = inst$rush$connector
      network_id = inst$rush$network_id

      while (!inst$is_terminated) {
        # sample new point xs
        xdt = private$.sampler$sample(1)$data
        xs = transpose_list(xdt)[[1]]

        # add unique id across stages, stage number, and budget
        asha_id = UUIDgenerate()
        xs = c(xs, list(asha_id = asha_id, stage = 1))
        xs[[budget_id]] = private$.r_min

        # evaluate
        ys = get_private(inst)$.eval_point(xs)

        # leaderboard
        r$ZADD(sprintf("%s:stage_1", network_id), ys[[1]], asha_id)

        # s_max is 0 if r_min == r_max
        if (s_max > 0) {
          # iterate stages
          for (s in seq(s_max)) {
            lg$debug("Fetching results from other workers")

            # fetch finished points of current stage
            # data_stage = archive$finished_data[list(s), , on = "stage"]

            # number of configurations in the current stage
            n_stage = r$ZCARD(sprintf("%s:stage_%i", network_id, s))

            # how many configurations can be promoted to the next stage
            # at least one configuration must be promotable
            n_promotable = max(floor(n_stage / eta), 1)

            lg$debug("%i promotable configurations in stage %i", n_promotable, s)

            # get the n best configurations of the current stage
            # candidates = private$.top_n(data_stage, archive$cols_y, n_promotable, direction)

            # ranks are 0-based and in ascending order
            rank = r$ZRANK(sprintf("%s:stage_%i", network_id, s), asha_id)

            if ((direction == 1 && rank >= n_promotable) || (direction == -1 && rank < n_stage - n_promotable)) {
              lg$debug("Configuration %s is not promotable to stage %i - rank %i", asha_id, s + 1, rank)
              break
            }

            lg$debug("Configuration %s is promotable to stage %i - rank %i", asha_id, s + 1, rank)

            # increase budget of xs
            rs = r_min * eta^s
            if (inst$search_space$class[[budget_id]] == "ParamInt") rs = round(rs)
            xs[[budget_id]] = rs
            xs$stage = s + 1

            # evaluate
            ys = get_private(inst)$.eval_point(xs)
            r$ZADD(sprintf("%s:stage_%i", network_id, s + 1), ys[[1]], asha_id)
          }
        }
      }
    }
  )
)

mlr_optimizers$add("async_successive_halving2", OptimizerAsyncSuccessiveHalving2)
