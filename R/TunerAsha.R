#' @title Tuner using Asynchronous Successive Halving
#'
#' @export
TunerAsha = R6Class("TunerAsha",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerAsha$new()
      )
    },

    optimize = function(inst) {
      assert_multi_class(inst, c("TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))

      # replace default with async objective
      inst$objective = ObjectiveTuningAsync$new(task = inst$objective$task, learner = inst$objective$learner,
        resampling = inst$objective$resampling, measures = inst$objective$measures,
        store_benchmark_result = inst$objective$store_benchmark_result, store_models = inst$objective$store_models,
        check_values = inst$objective$check_values, allow_hotstart = inst$objective$allow_hotstart)

      private$.optimizer$optimize(inst)

      # combine resample results to single benchmark result
      if (inst$objective$store_benchmark_result) {
        rdatas = map(unlist(inst$archive$data$resample_result), function(rr) get_private(rr)$.data)
        rdata = Reduce(function(x, y) x$combine(y), rdatas)
        inst$archive$benchmark_result = BenchmarkResult$new(rdata)
        inst$archive$data["evaluated", uhash := inst$archive$benchmark_result$uhashes, on = "status"]
      }

      # remove resample result column
      if (!is.null(inst$archive$data$resample_result)) inst$archive$data[, resample_result := NULL]
    }
  )
)
