#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import mlr3tuning
#' @import ggplot2
#' @importFrom R6 R6Class
"_PACKAGE"


.onLoad = function(libname, pkgname) {

  # add hyperband to sugar
  x = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
  x$add("hyperband", TunerHyperband)
  assign(
    "lg",
    # this seems extremely inconvenient
    # as changing the level of mlr3 or ml3/mlr3tuning would also adjust
    # the hyperband logger
    lgr::get_logger("mlr3/mlr3tuning/mlr3hyperband"),
    envir = parent.env(environment())
  )

  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }

  lgr::get_logger("mlr3")$set_threshold("warn")
  lg$set_threshold("info")

  # use custom logging level for hyperband between "warn" and "info" level
  # lgr::add_log_levels(c("info hb" = 350))
  # ignore "info" level loggings as they clutter the logs too much
  # lg$set_threshold("info hb")
} # nocov end
