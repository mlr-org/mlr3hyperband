#' @param eta (`numeric(1)`)\cr
#' Fraction parameter of the successive halving algorithm: With every stage the
#' configuration budget is increased by a factor of `eta` and only the best
#' `1/eta` points are used for the next stage. Non-integer values are supported,
#' but `eta` is not allowed to be less or equal 1.
