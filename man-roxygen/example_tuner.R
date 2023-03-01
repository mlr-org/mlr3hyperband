#' @examples
#' if(requireNamespace("xgboost")) {
#'   library(mlr3learners)
#'
#'   # define hyperparameter and budget parameter
#'   search_space = ps(
#'     nrounds = p_int(lower = 1, upper = 16, tags = "budget"),
#'     eta = p_dbl(lower = 0, upper = 1),
#'     booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
#'   )
#'
#'   \donttest{
#'   # hyperparameter tuning on the pima indians diabetes data set
#'   instance = tune(
#'     tnr("<%= id %>"),
#'     task = tsk("pima"),
#'     learner = lrn("classif.xgboost", eval_metric = "logloss"),
#'     resampling = rsmp("cv", folds = 3),
#'     measures = msr("classif.ce"),
#'     search_space = search_space,
#'     term_evals = 100
#'   )
#'
#'   # best performing hyperparameter configuration
#'   instance$result
#'   }
#' }
