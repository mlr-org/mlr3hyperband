#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import mlr3tuning
#' @import bbotk
#' @importFrom R6 R6Class
#' @importFrom utils head
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  # add hyperband to tuner dictionary
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  x$add("hyperband", TunerHyperband)
  x$add("successive_halving", TunerSuccessiveHalving)
  x$add("asha", TunerAsha)
  x$add("ahb", TunerAhb)

  # add hyperband to optimizer dictionary
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  x$add("hyperband", OptimizerHyperband)
  x$add("successive_halving", OptimizerSuccessiveHalving)
  x$add("asha", OptimizerAsha)
  x$add("ahb", OptimizerAhb)

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
