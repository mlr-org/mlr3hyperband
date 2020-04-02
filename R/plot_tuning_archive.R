#' @title Plot for TuningInstance
#'
#' @description
#' Generates plots for [mlr3tuning::TuningInstance], depending on argument `type`:
#'
#' * `"boxplot"` (default): Boxplots of evaluated performance for the
#' respective hyperparameter configurations if tuning was performed by [mlr3hyperband::TunerHyperband]. Boxplots are shown in facets per bracket. Additionaly, the budget spent is shown.
#'
#' @param object ([mlr3::TuningInstance]).
# @param measure ([mlr3::Measure]).
#' @param ... (`any`):
#'   Additional arguments, passed down to the respective `geom`.
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3hyperband)
#'
#' task = tsk("pima")
#' learner = lrn("classif.xgboost")
#' resampling = rsmp("holdout")
#' measure = msr("classif.acc")
#'
#' ps = ParamSet$new(
#'    params = list(
#'       ParamInt$new("nrounds", lower = 1L, upper = 270L, tags = "budget"),
#'       ParamInt$new("max_depth", lower = 1, upper = 100)
#'    )
#' )
#' term = term("evals", n_evals = 100000L)
#' inst = TuningInstance$new(task, learner, resampling, measure, ps, term)
#' tuner = tnr("hyperband", eta = 3L)
#'
#' head(fortify(object))
#' autoplot(object)
#' autoplot(object$clone()$filter(task_ids = "spam"), type = "roc")
#' autoplot(object$clone()$filter(task_ids = "pima"), type = "prc")
autoplot.TuningInstance = function(object, # nolint
  type = "boxplot",
  measure = NULL,
  ...) {

  assert_string(type)

  tab = fortify(object)

  switch(type,
    "boxplot" = {
      tabm = reshape2::melt(tab[, c("bracket", "bracket_stage", "budget_real", "n_configs")], measure.vars = c("budget_real", "n_configs"))

      p1 = ggplot(tab, mapping = aes_string(x = "bracket_stage", y = measure_id)) + geom_boxplot()
      p1 = p1 + facet_grid(cols = vars(bracket), labeller = label_both) + theme_bw()
      p2 = ggplot(tabm, mapping = aes(x = bracket_stage, y = value, fill = variable)) + geom_bar(stat = 'identity', position = "dodge")
      p2 = p2 + facet_grid(cols = vars(bracket), labeller = label_both) + theme_bw() + theme(legend.position = "bottom")
      gridExtra::grid.arrange(p1, p2, nrow = 2)
    },

    "surface" = {
      require_namespaces("precrec")
      autoplot(precrec::evalmod(as_precrec(object)), curvetype = "ROC",
        show_cb = TRUE)
    },

    "prc" = {
      require_namespaces("precrec")
      autoplot(precrec::evalmod(as_precrec(object)), curvetype = "PRC",
        show_cb = TRUE)
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
fortify.TuningInstance = function(model, data = NULL, measure = NULL, ...) { # nolint
  archive = model$archive()
  archive = cbind(archive, do.call(rbind, archive$tune_x))
  archive$tune_x = NULL
  # archive = cbind(archive, do.call(rbind, archive$params))
  archive$params = NULL
  archive$bracket_stage = as.factor(archive$bracket_stage)
  archive$bracket = as.factor(archive$bracket)

  return(archive)
  # archive = cbind(archive, archi)
  # measure = mlr3::assert_measure(mlr3::as_measure(measure,
  #   task_type = task$task_type), task = task)
  # model$score(measures = measure)[, c("nr", "task_id", "learner_id",
  #   "resampling_id", measure$id), with = FALSE]
}
