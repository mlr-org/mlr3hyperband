#' @include aaa.R
#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import mlr3tuning
#' @import bbotk
#' @importFrom R6 R6Class
#' @importFrom utils head
#' @importFrom uuid UUIDgenerate
"_PACKAGE"

register_bbotk = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  iwalk(optimizers, function(obj, nm) x$add(nm, obj))
} # nocov end

register_mlr3tuning = function() {
  # nocov start
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  iwalk(tuners, function(obj, nm) x$add(nm, obj))
} # nocov end

.onLoad = function(libname, pkgname) { # nolint
  register_namespace_callback(pkgname, "bbotk", register_bbotk)
  register_namespace_callback(pkgname, "mlr3tuning", register_mlr3tuning)

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

.onUnload = function(libpaths) { # nolint
  walk(names(optimizers), function(id) bbotk::mlr_optimizers$remove(id))
  walk(names(tuners), function(id) mlr3tuning::mlr_tuners$remove(id))
}

leanify_package()
