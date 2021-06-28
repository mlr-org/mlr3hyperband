#' @section Runtime:
#' The calculation of each bracket currently assumes a linear runtime in the
#' chosen budget parameter is always given. Hyperband is designed so each
#' bracket requires approximately the same runtime as the sum of the budget
#' over all configurations in each bracket is roughly the same. This will not
#' hold true once the scaling in the budget parameter is not linear
#' anymore, even though the sum of the budgets in each bracket remains the
#' same. A possible adaption would be to introduce a trafo, like it is shown in
#' the *examples* section.
#'
