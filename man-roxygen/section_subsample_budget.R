#' @section Subsample Budget:
#' If the learner lacks a natural budget parameter,
#' [mlr3pipelines::PipeOpSubsample] can be applied to use the subsampling rate
#' as budget parameter. The resulting [mlr3pipelines::GraphLearner] is fitted on
#' small proportions of the [mlr3::Task] in the first stage, and on the complete
#' task in last stage.
