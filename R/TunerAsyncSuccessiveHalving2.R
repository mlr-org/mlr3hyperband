#' @export
TunerAsyncSuccessiveHalving2 = R6Class("TunerAsyncSuccessiveHalving2",
  inherit = TunerAsyncFromOptimizerAsync,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsyncSuccessiveHalving2$new(),
        man = "mlr3hyperband::mlr_tuners_async_successive_halving2"
      )
    }
  )
)

#' @include aaa.R
tuners[["async_successive_halving2"]] = TunerAsyncSuccessiveHalving2
