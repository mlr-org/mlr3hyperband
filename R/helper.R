#' @title Hyperband Schedule
#'
#' @description
#' Calculates hyperband schedule.
#'
#' @param r_min (`numeric(1)`)\cr
#' Lower bound of budget parameter.
#' @param r_max (`numeric(1)`)\cr
#' Upper bound of budget parameter.
#' @param eta (`numeric(1)`)\cr
#' Fraction parameter of the successive halving algorithm: With every stage the
#' configuration budget is increased by a factor of `eta` and only the best
#' `1/eta` points are used for the next stage. Non-integer values are supported,
#' but `eta` is not allowed to be less or equal 1.
#' @param round (`logical(1)`)\cr
#' Determines if budget is an integer.
#'
#' @return [data.table::data.table()]
#' @export
hyperband_schedule = function(r_min, r_max, eta, round = FALSE) {
  r = r_max / r_min
  s_max = floor(log(r, eta))
  b = (s_max + 1) * r

  map_dtr(s_max:0, function(s) {
    nb = ceiling((b / r) * ((eta^s) / (s + 1)))
    rb = r * eta^(-s)
    map_dtr(0:s, function(i) {
      ni = floor(nb * eta^(-i))
      ri = r_min * rb * eta^i
      if (round) ri = round(ri)
      data.table(bracket = s, stage = i, budget = ri, n = ni)
    })
  })
}

#' @title Hyperband Schedule
#'
#' @description
#' Calculates how many different configurations are sampled.
#'
#' @param r_min (`numeric(1)`)\cr
#' Lower bound of budget parameter.
#' @param r_max (`numeric(1)`)\cr
#' Upper bound of budget parameter.
#' @param eta (`numeric(1)`)\cr
#' Fraction parameter of the successive halving algorithm: With every stage the
#' configuration budget is increased by a factor of `eta` and only the best
#' `1/eta` points are used for the next stage. Non-integer values are supported,
#' but `eta` is not allowed to be less or equal 1.
#' @param round (`logical(1)`)\cr
#' Determines if budget is an integer.
#'
#' @return `integer(1)`
#' @export
hyperband_n_configs = function(r_min, r_max, eta) {
  r = r_max / r_min
  s_max = floor(log(r, eta))
  budget = (s_max + 1) * r

  sum(ceiling((budget / r) * (eta^(s_max:0)) / (s_max:0 + 1)))
}

#' @title Hyperband Budget
#'
#' @description
#' Calculates the total budget used by hyperband.
#'
#' @param r_min (`numeric(1)`)\cr
#' Lower bound of budget parameter.
#' @param r_max (`numeric(1)`)\cr
#' Upper bound of budget parameter.
#' @param eta (`numeric(1)`)\cr
#' Fraction parameter of the successive halving algorithm: With every stage the
#' configuration budget is increased by a factor of `eta` and only the best
#' `1/eta` points are used for the next stage. Non-integer values are supported,
#' but `eta` is not allowed to be less or equal 1.
#'
#' @return `integer(1)`
#' @export
hyperband_bduget = function(r_min, r_max, eta, round = FALSE) {
  schedule = hyperband_schedule(r_min, r_max, eta, round)
  sum(schedule[, budget * n])
}



     
