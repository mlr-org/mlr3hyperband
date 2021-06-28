#' @section Parallelization:
#' In order to support general termination criteria and parallelization, we
#' evaluate points in a batch-fashion of size `batch_size`. The points of one
#' stage in a bracket are evaluated in one batch. Parallelization is supported
#' via package \CRANpkg{future} (see [mlr3::benchmark()]'s section on
#' parallelization for more details).
