#' @title MeasureClassifBudget
#' 
#' @noRd 
#' 
#' @description 
#' Scores is drawn from a normal distribution with a mean of 
#'  `learner$param_set$values[[budget_id]]`.
#' 
#' @param budget_id (`character(1)`)\cr
#'  Id of the learner's budget parameter.
MeasureClassifBudget = R6Class("MeasureClassifBudget",
  inherit = MeasureClassif,
  public = list(
    budget_id = NULL,

    initialize = function(budget_id) {
      self$budget_id = budget_id
      super$initialize(
        id = "budget",
        range = c(-Inf, Inf),
        minimize = FALSE,
        properties = "requires_learner"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, ...) {
      rnorm(1, mean = learner$param_set$values[[self$budget_id]])
    }
  )
)

mlr_measures$add("budget", MeasureClassifBudget)
