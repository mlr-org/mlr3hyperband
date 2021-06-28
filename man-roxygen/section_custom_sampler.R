#' @section Custom sampler:
#' Hyperband supports custom [paradox::Sampler] object for initial
#' configurations in each bracket.
#' A custom sampler may look like this (the full example is given in the
#' *examples* section):
#' ```
#' # - beta distribution with alpha = 2 and beta = 5
#' # - categorical distribution with custom probabilities
#' sampler = SamplerJointIndep$new(list(
#'   Sampler1DRfun$new(params[[2]], function(n) rbeta(n, 2, 5)),
#'   Sampler1DCateg$new(params[[3]], prob = c(0.2, 0.3, 0.5))
#' ))
#' ```
