#' @title Optimizer using the Hyperband algorithm
#'
#' @name mlr_optimizers_hyperband
#'
#' @description
#' `OptimizerHyperband` class that implements hyperband optimization. HyperbandX
#' (HBX) repeatedly calls SH ([OptimizerSuccessiveHalving]) with different
#' numbers of starting points. A larger number of starting points corresponds to
#' a smaller budget allocated in the base stage. Each run of SH within HBX is
#' called a bracket. HBX considers `s_max + 1` brackets with `s_max =
#' floor(log(r_max / r_min, eta)`. The most explorative bracket `s = s_max`
#' constructs `s_max + 1` stages and allocates the minimum budget (`r_min`) in
#' the base stage. The minimum budget is increased in each bracket by a factor
#' of `eta` and the number of starting points is computed so that each bracket
#' approximately spends the same budget. Use [hyperband_schedule()] to get a
#' preview of the bracket layout.
#'
#' |   s |     |   3 |     |     |   2 |     |     |   1 |     |     |   0 |
#' | ---:| ---:| ---:| --- | ---:| ---:| --- | ---:| ---:| --- | ---:| ---:|
#' |   i |  ni |  ri |     |  ni |  ri |     |  ni |  ri |     |  ni |  ri |
#' |   0 |   8 |   1 |     |   6 |   2 |     |   4 |   4 |     |   8 |   4 |
#' |   1 |   4 |   2 |     |   3 |   4 |     |   2 |   8 |     |     |     |
#' |   2 |   2 |   4 |     |   1 |   8 |     |     |     |     |     |     |
#' |   3 |   1 |   8 |     |     |     |     |     |     |     |     |     |
#'
#' The budget hyperparameter must be tagged with `"budget"` in the search space.
#' The minimum budget (`r_min`) which is allocated in the base stage of the most
#' explorative bracket, is set by the lower bound of the budget parameter. The
#' upper bound defines the maximum budget (`r_max`) which which is allocated to
#' the candidates in the last stages.
#'
#' @templateVar id hyperband
#' @template section_dictionary_optimizers
#'
#' @section Parameters:
#' \describe{
#' \item{`eta`}{`numeric(1)`\cr
#' With every stage, the budget is increased by a factor of `eta`
#' and only the best `1 / eta` points are promoted to the next stage.
#' Non-integer values are supported, but `eta` is not allowed to be less or
#' equal 1.
#' }
#' \item{`sampler`}{[paradox::Sampler]\cr
#' Object defining how the samples of the parameter space should be drawn in the
#' base stage of each bracket. The default is uniform sampling.
#' }
#' \item{`repeats`}{`logical(1)`\cr
#' If `FALSE` (default), hyperband terminates once all brackets are evaluated.
#' Otherwise, hyperband starts over again once the last bracket is evaluated.
#' }}
#'
#' @section Archive:
#' The [bbotk::Archive] holds the following additional columns that are specific
#' to the hyperband algorithm:
#'   * `bracket` (`integer(1)`)\cr
#'     The bracket index. Counts down to 0.
#'   * `stage` (`integer(1))`\cr
#'     The stages of each bracket. Starts counting at 0.
#'
#' @template section_custom_sampler
#' @template section_progress_bars
#' @template section_logging
#
#' @source
#' `r format_bib("li_2018")`
#'
#' @export
#' @examples
#' library(bbotk)
#' library(data.table)
#'
#' search_space = domain = ps(
#'   x1 = p_dbl(-5, 10),
#'   x2 = p_dbl(0, 15),
#'   fidelity = p_dbl(1e-2, 1, tags = "budget")
#' )
#'
#' # modified branin function
#' objective = ObjectiveRFunDt$new(
#'   fun = function(xdt) {
#'     a = 1
#'     b = 5.1 / (4 * (pi ^ 2))
#'     c = 5 / pi
#'     r = 6
#'     s = 10
#'     t = 1 / (8 * pi)
#'     data.table(y =
#'       (a * ((xdt[["x2"]] -
#'       b * (xdt[["x1"]] ^ 2L) +
#'       c * xdt[["x1"]] - r) ^ 2) +
#'       ((s * (1 - t)) * cos(xdt[["x1"]])) +
#'       s - (5 * xdt[["fidelity"]] * xdt[["x1"]])))
#'   },
#'   domain = domain,
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#' )
#'
#' instance = OptimInstanceSingleCrit$new(
#'   objective = objective,
#'   search_space = search_space,
#'   terminator = trm("none")
#' )
#'
#' optimizer = opt("hyperband")
#'
#' # modifies the instance by reference
#' optimizer$optimize(instance)
#'
#' # best scoring evaluation
#' instance$result
#'
#' # all evaluations
#' as.data.table(instance$archive)
OptimizerHyperband = R6Class("OptimizerHyperband",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta     = p_dbl(lower = 1.0001, tags = "required", default = 2),
        sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        repeats = p_lgl(default = FALSE)
      )
      param_set$values = list(eta = 2, sampler = NULL, repeats = FALSE)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        param_set = param_set,
        properties = c("dependencies", "single-crit", "multi-crit"),
        packages = "mlr3hyperband"
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

      # r_min is the budget of a single configuration in the first stage
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

      repeat({
        # original hyperband algorithm iterates over brackets
        # this implementation iterates over stages with same budget
        # the number of iterations (s_max + 1) remains the same in both implementations
        for (s in s_max:0) {
          # budget of a single configuration in the first stage (unscaled)
          rs = r_min * r * eta^(-s)
          # sample initial configurations of bracket
          xdt = sampler$sample(n[s + 1])$data
          set(xdt, j = budget_id, value = rs)
          set(xdt, j = "bracket", value = s)
          set(xdt, j = "stage", value = 0)

          # promote configurations of previous batch
          if (s != s_max) {
            archive = inst$archive
            data = archive$data[batch_nr == archive$n_batch, ]
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
            xdt_promoted = xdt_promoted[, c(inst$archive$cols_x, "stage", "bracket"), with = FALSE]
            set(xdt_promoted, j = budget_id, value = rs)
            set(xdt_promoted, j = "stage", value = xdt_promoted[["stage"]] + 1)

            xdt = rbindlist(list(xdt, xdt_promoted), use.names = TRUE)
          }

          if (search_space$class[[budget_id]] == "ParamInt") set(xdt, j = budget_id, value = round(xdt[[budget_id]]))
          inst$eval_batch(xdt)
        }
        if (!pars$repeats) break
      })
    }
  )
)
