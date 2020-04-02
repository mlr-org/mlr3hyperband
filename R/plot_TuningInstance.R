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
autoplot.TuningInstance = function(object, # nolint
  type = "boxplot",
  measure = NULL,
  hb_bracket = NULL,
  params = NULL, 
  facet_per_bracket = TRUE,
  ...) {

  assert_string(type)
  assert_integer(hb_bracket, null.ok = TRUE)

  measures = object$measures
  measure_ids = unlist(lapply(measures, function(x) x$id))
  assert_choice(measure, measure_ids, null.ok = TRUE)

  if (!is.null(measure))
    measure = measures[[measure %in% measure_ids]]
  else 
    measure = measures[[1]]

  measure_id = measure$id

  ps = object$param_set
  pids = ps$ids()
  assert_subset(params, pids, empty.ok = TRUE)
  assert_param_set(ps, c("ParamDbl", "ParamInt"))

  if (is.null(params)) {
    params = pids[1:2]
  }

  tab = fortify(model = object, measure = measure, params = params)
  if (!is.null(hb_bracket))
    tab = tab[bracket == hb_bracket, ]

  switch(type,
    "boxplot" = {
      p = ggplot(tab, mapping = aes_string(x = "bracket_stage", y = measure_ids[[1]])) + geom_boxplot()
      p = p + geom_point(data = tab[isbest == TRUE, ], aes_string(x = "bracket_stage", y = measure_ids[[1]]), colour = "blue")
      if (facet_per_bracket)
        p = p + facet_grid(cols = vars(bracket), labeller = label_both) 
      p + theme_bw()      
    },

    "budget" = {
      p = ggplot() 
      p = p + geom_bar(tab, mapping = aes(x = bracket_stage, y = budget_real, fill = classif.acc), stat = "identity", colour = "white")
      if (facet_per_bracket)
        p = p + facet_grid(cols = vars(bracket), labeller = label_both) 
      p + theme_bw()      
    },

    "input" = {

      if (length(params) == 1L) {
        p = ggplot(tab, aes_string(x = params, y = measure_id))
        p = p + geom_line() + geom_point()
      }
      if (length(params) > 1L) {
        p = ggplot(tab, aes_string(x = params[1], y = params[2], colour = measure_id))
        p = p + geom_point()
      }
      if (facet_per_bracket)
        p = p + facet_grid(cols = vars(bracket), labeller = label_both) 
      p + theme_bw()      
    },

    stopf("Unknown plot type '%s'", type)
  )
}

#' @export
fortify.TuningInstance = function(model, measure, params = NULL, ...) { # nolint
  minimize = measure$minimize
  mid = measure$id 

  archive = model$archive()
  archive = cbind(archive, do.call(rbind, archive$tune_x))
  archive$tune_x = NULL
  # archive = cbind(archive, do.call(rbind, archive$params))
  archive$params = NULL
  archive$bracket_stage = as.factor(archive$bracket_stage)
  archive$bracket = factor(archive$bracket, levels = sort(unique(archive$bracket), decreasing = TRUE))

  if (minimize) 
    archive[measure$id] = archive[measure$id] * - 1 

  archive[, isbest := (get(mid) == max(get(mid))), by = c("bracket", "bracket_stage")]

  if (minimize) 
    archive[measure$id] = archive[measure$id] * - 1 

  if (!is.null(params))
    archive[, (params) := lapply(.SD, as.numeric), .SDcols = params]

  return(archive)
}
