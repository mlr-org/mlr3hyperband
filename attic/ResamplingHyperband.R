
#' @title Hyperband Resampling
#'
#' @usage NULL
#' @aliases mlr_resamplings_hyperband
#' @format [R6::R6Class] inheriting from [ResamplingHoldout].
#' @include Resampling.R
#'
#' @section Construction:
#' ```
#' ResamplingHyperband$new()
#' mlr_resamplings$get("hyperband")
#' rsmp("hyperband")
#' ```
#'
#' @description
#' Uses subset of original data based on the given budget as training set
#' Parameter `ratio` determines the ratio of observation going into the training set (default: 2/3).
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @template seealso_resampling
#' @export
#' @examples
#'
#' # Internal storage:
#' rho$instance # simple list
ResamplingHyperband = R6Class(
  "ResamplingHyperband",
  inherit = ResamplingHoldout,

  public = list(
    budget = 1,

    initialize = function(id = "hyperband", param_vals = list(ratio = 2 / 3)) {
      super$initialize()
    },

    train_set = function(i) {

      set = super$train_set(i)
      n = round(length(set) * self$budget)
      assert_integerish(n, lower = 1)
      set = set[seq_len(n)]
      return(set)

    }
  )
)
#' @include mlr_resamplings.R
mlr_resamplings$add("hyperband", ResamplingHyperband)
