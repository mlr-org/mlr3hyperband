 search_space = domain = ps(
  x1 = p_dbl(-5, 10),
  x2 = p_dbl(0, 15),
  fidelity = p_dbl(1e-2, 1, tags = "budget")
)

# modified branin function
objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    Sys.sleep(max(1, round(rnorm(n = 1, mean = xdt[["fidelity"]] * 1000, s = 3))))

    a = 1
    b = 5.1 / (4 * (pi ^ 2))
    c = 5 / pi
    r = 6
    s = 10
    t = 1 / (8 * pi)
    data.table::data.table(y =
      (a * ((xdt[["x2"]] -
      b * (xdt[["x1"]] ^ 2L) +
      c * xdt[["x1"]] - r) ^ 2) +
      ((s * (1 - t)) * cos(xdt[["x1"]])) +
      s - (5 * xdt[["fidelity"]] * xdt[["x1"]])))
  },
  domain = domain,
  codomain = ps(y = p_dbl(tags = "minimize"))
)

instance = OptimInstanceSingleCrit$new(
  objective = objective,
  search_space = search_space,
  terminator = trm("none")
)

optimizer = opt("asha")

optimizer$optimize(instance)
