#' @section Dictionary:
#' This [mlr3tuning::Tuner] can be instantiated via the [dictionary][mlr3misc::Dictionary]
#' [mlr3tuning::mlr_tuners] or with the associated sugar function [mlr3tuning::tnr()]:
#' ```
#' <%= class(tnr(id))[1] %>$new()
#' mlr_tuners$get("<%= id %>")
#' tnr("<%= id %>")
#' ```
