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
  assign("lg", lgr::get_logger("mlr3/mlr3tuning"), envir = parent.env(environment()))
} # nocov end
