test_that("OptimizerSuccessiveHalving works async", {
  search_space = domain = ps(
    x1 = p_dbl(-5, 10),
    x2 = p_dbl(0, 15),
    fidelity = p_dbl(1e-2, 1, tags = "budget")
  )

  branin2 = function(xs) {
    Sys.sleep(5)
    list(y = (xs[["x2"]] - ((5.1/(4 * pi^2)) - 0.1 * (1 - xs[["fidelity"]])) *
        xs[["x1"]]^2 + (5/pi) * xs[["x1"]] - 6)^2 + 10 * (1 -
        (1/(8 * pi))) * cos(xs[["x1"]]) + 10)
  }

  # objective with modified branin function, see `bbotk::branin()`
  objective = ObjectiveRFun$new(
    fun = branin2,
    domain = domain,
    codomain = ps(y = p_dbl(tags = "minimize"))
  )

  # initialize instance and optimizer
  instance = OptimInstanceSingleCrit$new(
    objective = objective,
    search_space = search_space,
    terminator = trm("evals", n_evals = 50)
  )

  opt("successive_halving", async = TRUE)

  future::plan("multisession", workers = 8)

  # optimize branin function
  optimizer$optimize(instance)
})
