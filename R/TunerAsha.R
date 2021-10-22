#' @title Tuner using Asynchronous Successive Halving
#'
#' @export
TunerAsha = R6Class("TunerAsha",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsha$new()
      )
    }
  )
)
