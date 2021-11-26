#' @examples
#' library(bbotk)
#' library(data.table)
#'
#' # set search space
#' search_space = domain = ps(
#'   x1 = p_dbl(-5, 10),
#'   x2 = p_dbl(0, 15),
#'   fidelity = p_dbl(1e-2, 1, tags = "budget")
#' )
#'
#' # objective with modified branin function, see `bbotk::branin()`
#' objective = ObjectiveRFun$new(
#'   fun = branin,
#'   domain = domain,
#'   codomain = ps(y = p_dbl(tags = "minimize"))
#' )
#'
#' # initialize instance and optimizer
#' instance = OptimInstanceSingleCrit$new(
#'   objective = objective,
#'   search_space = search_space,
#'   terminator = trm("evals", n_evals = 50)
#' )
#'
#' optimizer = opt("<%= id %>")
#'
#' # optimize branin function
#' optimizer$optimize(instance)
#'
#' # best scoring evaluation
#' instance$result
#'
#' # all evaluations
#' as.data.table(instance$archive)
