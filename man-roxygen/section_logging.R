#' @section Logging:
#'
#' \CRANpkg{mlr3hyperband} uses the \CRANpkg{lgr} package for logging.
#' \CRANpkg{lgr} supports multiple log levels which can be queried with
#' `getOption("lgr.log_levels")`. Use `lgr::get_logger("bbotk")` to access and
#' control the logger.
#'
#' To suppress output and reduce verbosity, you can lower the log from the
#' default level `"info"` to `"warn"`:
#'
#' ```
#' lgr::get_logger("bbotk")$set_threshold("warn")
#' ```
#'
#' To log to a file or a data base, see the documentation of [lgr::lgr-package].
