#' @title Tuner using the Successive Halving algorithm
#'
#' @name mlr_tuners_successivehalving

TunerSuccessiveHalving = R6Class("TunerSuccessiveHalving",
  inherit = Tuner,
  public = list(

    mo_archive = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      ps = ParamSet$new(list(
        ParamDbl$new("eta", lower = 1.0001, tags = "required", default = 2),
        ParamUty$new("sampler",
          custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        ParamInt$new("n", lower = 1), # TODO: Check if boundaries are correct
        ParamDbl$new("r", lower = 1e-8), # TODO: Check if boundaries are correct
        ParamFct$new("mo_method", levels = c("indicator_based", "dominance_based", "scalarized"), default = "dominance_based"),
        ParamFct$new("tie_breaker", levels = c("CD", "HV"), default = "CD"),
        ParamInt$new("np", lower = 1L)
      ))

      ps$values = list(eta = 2, sampler = NULL)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = ps,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = character(0)
      )
    },

    optimize = function(inst) {
      assert_multi_class(inst, c("OptimInstanceMultiCrit", "OptimInstanceSingleCrit", "TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))
      optimize_default(inst, self, private)
    }
  ),

  private = list(
    .optimize = function(inst) {

      pars = self$param_set$values
      n = pars$n
      np = pars$np
      r_min = pars$r

      eta = pars$eta
      sampler = pars$sampler
      ps = inst$search_space
      budget_id = ps$ids(tags = "budget")
      assert_character(budget_id, len = 1)

      self$mo_archive = NULL

      mo_method = pars$mo_method
      tie_breaker = pars$tie_breaker

      ps_sampler = ps$clone()$subset(setdiff(ps$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(ps_sampler)
      }

      # Check if r is chosen properly
      assert_number(r_min, lower = ps$lower[[budget_id]], upper = ps$upper[[budget_id]], null.ok = TRUE)

      if (is.null(r_min))
        r_min = ps$lower[[budget_id]]

      r_max = ps$upper[[budget_id]]

      # Budget needs to be numeric
      assert_choice(ps$class[[budget_id]], c("ParamInt", "ParamDbl"))


      outer_iters = ifelse(mo_method == "dominance_based", 1L, np)
      reduce_to = ifelse(mo_method == "dominance_based", np, 1L)

      # Number of stages if each configuration in the fist stage uses r_min
      # resources and each configuration in the last stage uses not more than
      # r_max resources
      k_r = floor(log(r_max / r_min, eta))

      # Number of stages so that the last stages evaluates more than 1
      # configuration
      k_n = floor(log(n / reduce_to, eta))

      # The number of halvings we can perform
      k = min(k_n, k_r)

      # Budget used (each step evaluates (n * eta^(-s)) x r * eta^s evalations)
      B = n * r_min * (k + 1) * outer_iters

      lg$info("Performing Successive Halving with a total budget of %g.", B)

      design = data.table(n = floor(n * eta^(- seq(0, k))), r = r_min * eta^seq(0, k))
      print("Design of the bracket:")
      print(design)

      mo_archive = NULL

      for (o in seq_len(outer_iters)) {

        for(i in seq(0, k)) {

          ni = ceiling(n * eta^(-i))
          ri = r_min * eta^i

          if (i == 0) {
            xdt = sampler$sample(ni)$data
            xdt$continue_hash = seq(nrow(xdt))
          } else {
            archive = inst$archive
            data = archive$data[batch_nr %in% archive$n_batch]
            y = data[, archive$cols_y, with = FALSE]
            minimize = mult_max_to_min(archive$codomain) == 1

            if (archive$codomain$length == 1) {
              row_ids = tail(order(y, decreasing = minimize), ni)
            } else {
              row_ids = select_survivors(points = y, n_select = ni,
                minimize = minimize, method = mo_method, tie_breaker = tie_breaker,
                archive = self$mo_archive[, inst$objective$codomain$ids(), with = FALSE])
            }
            xdt = data[row_ids, c(archive$cols_x, "continue_hash"), with = FALSE]
          }

          xdt[[budget_id]] = ri
          xdt$stage = i + 1
          xdt$outer_stage = o
          inst$eval_batch(xdt)
        }

        if (is.null(self$mo_archive)) {
          self$mo_archive = inst$archive$data[stage == i + 1 & outer_stage == o, ]
          self$mo_archive = undominated(self$mo_archive, inst$objective$codomain$ids())
        } else {
          self$mo_archive = rbind(self$mo_archive, inst$archive$data[stage == i + 1 & outer_stage == o, ])
          self$mo_archive = undominated(self$mo_archive, inst$objective$codomain$ids())
        }
      }
    },
    .assign_result = function(inst) {
      assert_multi_class(inst, c("OptimInstanceMultiCrit", "OptimInstanceSingleCrit", "TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))
      assign_result_default(inst)
    }
  )
)
