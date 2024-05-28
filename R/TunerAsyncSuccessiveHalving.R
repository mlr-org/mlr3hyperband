#' @export
TunerAsyncSuccessiveHalving = R6Class("TunerAsyncSuccessiveHalving",
  inherit = TunerAsyncFromOptimizerAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsyncSuccessiveHalving$new(),
        man = "mlr3hyperband::mlr_tuners_async_successive_halving"
      )
    }
  )
)

#' @include aaa.R
tuners[["async_successive_halving"]] = TunerAsyncSuccessiveHalving
