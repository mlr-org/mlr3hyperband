% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TunerSuccessiveHalving.R
\name{mlr_tuners_successive_halving}
\alias{mlr_tuners_successive_halving}
\alias{TunerSuccessiveHalving}
\title{Hyperparameter Tuning with Successive Halving}
\source{
Jamieson K, Talwalkar A (2016).
\dQuote{Non-stochastic Best Arm Identification and Hyperparameter Optimization.}
In Gretton A, Robert CC (eds.), \emph{Proceedings of the 19th International Conference on Artificial Intelligence and Statistics}, volume 51 series Proceedings of Machine Learning Research, 240-248.
\url{http://proceedings.mlr.press/v51/jamieson16.html}.
}
\description{
\code{TunerSuccessiveHalving} class that implements the successive halving
algorithm.
}
\section{Parameters}{

\describe{
\item{\code{n}}{\code{integer(1)}\cr
Number of configurations in first stage.}
\item{\code{eta}}{\code{numeric(1)}\cr
With every step, the configuration budget is increased by a factor of \code{eta}
and only the best \code{1/eta} configurations are used for the next stage.
Non-integer values are supported, but \code{eta} is not allowed to be less or
equal 1.}
\item{\code{sampler}}{\link[paradox:Sampler]{paradox::Sampler}\cr
Object defining how the samples of the parameter space should be drawn during
the initialization of each bracket. The default is uniform sampling.}
}
}

\examples{
if(requireNamespace("xgboost")) {
library(mlr3hyperband)
library(mlr3learners)

# Define hyperparameter and budget parameter for tuning with hyperband
search_space = ParamSet$new(list(
  ParamInt$new("nrounds", lower = 1, upper = 4, tag = "budget"),
  ParamDbl$new("eta", lower = 0, upper = 1),
  ParamFct$new("booster", levels = c("gbtree", "gblinear", "dart"))
))

# Define termination criterion
# Successive halving terminates itself
terminator = trm("none")

# Create tuning instance
inst = TuningInstanceSingleCrit$new(
  task = tsk("iris"),
  learner = lrn("classif.xgboost"),
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  terminator = terminator,
  search_space = search_space,
)

# Load tuner
tuner = tnr("successive_halving", n = 16L, eta = 2L)

\donttest{
# Trigger optimization
tuner$optimize(inst)

# Print all evaluations
as.data.table(inst$archive)}
}
}
\section{Super class}{
\code{\link[mlr3tuning:Tuner]{mlr3tuning::Tuner}} -> \code{TunerSuccessiveHalving}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{TunerSuccessiveHalving$new()}}
\item \href{#method-clone}{\code{TunerSuccessiveHalving$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="format">}\href{../../mlr3tuning/html/Tuner.html#method-format}{\code{mlr3tuning::Tuner$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="optimize">}\href{../../mlr3tuning/html/Tuner.html#method-optimize}{\code{mlr3tuning::Tuner$optimize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3tuning" data-topic="Tuner" data-id="print">}\href{../../mlr3tuning/html/Tuner.html#method-print}{\code{mlr3tuning::Tuner$print()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TunerSuccessiveHalving$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TunerSuccessiveHalving$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}