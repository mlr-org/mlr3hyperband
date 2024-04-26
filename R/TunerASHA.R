#' @title Tuner using Asynchronous Successive Halving
#'
#' @description
#' Optimizer using the Asynchronous Successive Halving Algorithm (ASHA).
#'
#' @export
TunerASHA = R6Class("TunerASHA",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerASHA$new(),
        man = "mlr3hyperband::mlr_tuners_asha"
      )
    }
  )
)

#' @include aaa.R
tuners[["asha"]] = TunerASHA
