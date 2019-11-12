#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import mlr3tuning
#' @importFrom R6 R6Class
"_PACKAGE"


.onLoad = function(libname, pkgname) {
  # add hyperband to sugar
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  x$add("hyperband", TunerHyperband)
  assign(
    "lg",
    lgr::get_logger("mlr3/mlr3tuning"),
    envir = parent.env(environment())
  )

  # use custom logging level for hyperband between "warn" and "info" level
  lgr::add_log_levels(c("info hb" = 350))
  # ignore "info" level loggings as they clutter the logs too much
  lg$set_threshold("info hb")
} # nocov end
