#' @title Asynchronous Hyperparameter Tuning with Successive Halving
#'
#' @include OptimizerAsyncSuccessiveHalving.R
#' @name mlr_tuners_async_successive_halving
#' @templateVar id async_successive_halving
#'
#' @inherit mlr_optimizers_async_successive_halving description
#' @template section_dictionary_tuners
#' @inheritSection mlr_optimizers_async_successive_halving Parameters
#' @inheritSection mlr_optimizers_async_successive_halving Archive
#' @template section_subsample_budget
#' @template section_custom_sampler
#'
#' @source
#' `r format_bib("li_2020")`
#'
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
