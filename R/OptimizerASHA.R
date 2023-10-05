#' @title Optimizer using Asynchronous Successive Halving Algorithm
#'
#' @description
#' Optimizer using the Asynchronous Successive Halving Algorithm (ASHA).
#'
#' @export
OptimizerASHA = R6Class("OptimizerASHA",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        n                     = p_int(lower = 1, default = 16L),
        eta                   = p_dbl(lower = 1.0001, default = 2),
        sampler               = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        repetitions           = p_int(lower = 1L, default = 1L, special_vals = list(Inf)),
        adjust_minimum_budget = p_lgl(default = FALSE),
        hotstart              = p_lgl(default = FALSE)
      )
      param_set$values = list(n = 16L, eta = 2, sampler = NULL, repetitions = 1, adjust_minimum_budget = FALSE, hotstart = FALSE)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = param_set,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = "mlr3hyperband",
        label = "Asynchronous Successive Halving",
        man = "mlr3hyperband::mlr_optimizers_asha"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      stopf("%s must be started with `rush` to use %s", format(inst), format(self))
    },

    .optimize_async = function(inst) {
      if (inst$archive$codomain$length > 1) require_namespaces("emoa")
      pars = self$param_set$values
      rush = inst$rush
      archive = inst$archive
      search_space = inst$search_space
      sampler = pars$sampler
      budget_id = search_space$ids(tags = "budget")
      eta = pars$eta
      minimize = inst$archive$codomain$maximization_to_minimization == -1
      allow_hotstart = if (is.null(inst$objective$allow_hotstart)) FALSE else inst$objective$allow_hotstart

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

      # budget parameter
      if (length(budget_id) != 1) stop("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))
      integer_budget = search_space$class[[budget_id]] == "ParamInt"

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

      # increase r_min so that the last stage uses the maximum budget
      if (pars$adjust_minimum_budget) r_min = r_max / r_min * eta^-s_max

      # r_min is the budget of a single configuration in the base stage
      # r_max is the maximum budget of a single configuration in the top stage
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # s_max + 1 is the number of stages
      s_max = floor(log(r_max / r_min, eta))

      # increase r_min so that the last stage uses the maximum budget
      if (pars$adjust_minimum_budget) r_min = r_max / r_min * eta^-s_max

      repeat({
        n_free_workers = rush$n_workers - rush$n_running_tasks - rush$n_queued_tasks

        repeat({
          # repeat for each free worker
          if (n_free_workers <= 0) break
          xdt = NULL

          # fetch configurations with result
          # FIXME: exclude transformed xss
          data = rush$fetch_finished_tasks()

          # try to promote configuration
          # iterate stages from top to base stage
          for (s in (s_max - 1):-1) {

            if (s < 0 || !nrow(data)) {
              # no promotion possible
              # sample new configuration
              xdt = sampler$sample(1)$data
              if (integer_budget) r_min = as.integer(round(r_min))
              set(xdt, j = budget_id, value = r_min)
              set(xdt, j = "stage", value = 0L)
              set(xdt, j = "asha_id", value = uuid::UUIDgenerate())

              lg$debug("%s samples a new configuration %s.", format(self), as_short_string(as.list(xdt)))

              break
            }

            # configurations in stage
            data_stage = data[list(s), , on = "stage", nomatch = NULL]

            # no configurations in stage
            if (!nrow(data_stage)) next

            # top n configurations in stage
            y = data_stage[[archive$cols_y]]
            n = floor(nrow(data_stage) / eta)
            candidates = data_stage[top_n(y, n, minimize), ]

            # configuration in stage + 1
            next_stage = s + 1

            # check for queued, running and finished asha ids
            queued_asha_ids = rush$fetch_queued_tasks(fields = c("xs_extra"))$asha_id
            # only check priority queues if hotstarting is enabled
            priority_asha_ids = if (allow_hotstart) rush$fetch_priority_tasks(fields = c("xs_extra"))$asha_id else NULL
            running_asha_ids = rush$fetch_running_tasks(fields = c("xs_extra"))$asha_id
            finished_asha_ids = rush$fetch_finished_tasks()[list(next_stage), asha_id, on = "stage", nomatch = NULL]
            promotable_asha_ids = setdiff(candidates$asha_id, c(queued_asha_ids, priority_asha_ids, running_asha_ids, finished_asha_ids))

            # promote configuration
            if (length(promotable_asha_ids)) {

              lg$debug("%s promotes a configuration from stage %i.", format(self), s)

              ri = r_min * eta^(s + 1)
              if (integer_budget) ri = as.integer(round(ri))
              xdt = candidates[list(promotable_asha_ids[1]), c(archive$cols_x, "asha_id", "worker_id"), on = "asha_id", with = FALSE]
              set(xdt, j = budget_id, value = ri)
              set(xdt, j = "stage", value = s + 1)
              if (allow_hotstart) {
                lg$debug("%s requests hotstart.", format(self))
                setnames(xdt, "worker_id", "priority_id")
              } else xdt[, worker_id := NULL]
              break
            }
          }

          inst$eval_async(xdt)
          n_free_workers = n_free_workers - 1
        })
      })
    }
  )
)

#' @include aaa.R
optimizers[["asha"]] = OptimizerASHA
