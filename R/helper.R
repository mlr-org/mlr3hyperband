#' @title Hyperband Schedule
#'
#' @description
#' Returns hyperband schedule.
#'
#' @template param_r_min
#' @template param_r_max
#' @template param_eta
#' @template param_n_instances
#' @template param_integer_budget
#'
#' @return [data.table::data.table()]
#' @export
hyperband_schedule = function(r_min, r_max, eta, n_instances = 1, integer_budget = FALSE) {
  r = r_max / r_min
  s_max = floor(log(r, eta))
  b = (s_max + 1) * r

  map_dtr(s_max:0, function(s) {
    nb = ceiling((b / r) * ((eta^s) / (s + 1)))
    rb = r * eta^(-s)
    map_dtr(0:s, function(i) {
      ni = ceiling(nb * eta^(-i)) * n_instances
      ri = r_min * rb * eta^i
      if (integer_budget) ri = round(ri)
      data.table(bracket = s, stage = i, budget = ri, n = ni)
    })
  })
}

#' @title Hyperband Configs
#'
#' @description
#' Calculates how many different configurations are sampled.
#'
#' @template param_r_min
#' @template param_r_max
#' @template param_eta
#' @template param_n_instances
#'
#' @return `integer(1)`
#' @export
hyperband_n_configs = function(r_min, r_max, eta, n_instances = 1) {
  r = r_max / r_min
  s_max = floor(log(r, eta))
  budget = (s_max + 1) * r

  sum(ceiling((budget / r) * (eta^(s_max:0)) / (s_max:0 + 1))) * n_instances
}

#' @title Hyperband Budget
#'
#' @description
#' Calculates the total budget used by hyperband.
#'
#' @template param_r_min
#' @template param_r_max
#' @template param_eta
#' @template param_n_instances
#' @template param_integer_budget
#'
#' @return `integer(1)`
#' @export
hyperband_budget = function(r_min, r_max, eta, n_instances, integer_budget = FALSE) {
  schedule = hyperband_schedule(r_min, r_max, eta, integer_budget)
  sum(schedule[, get("budget") * get("n")]) * n_instances
}
