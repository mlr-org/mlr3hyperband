library(mlr3)
library(mlr3learners)
library(checkmate)
library(mlr3tuning)
library(paradox)
library(ggplot2)

context("plot_TuningInstance")

test_that("Plotting works", {
  
	ps = ParamSet$new(params = list(
	  ParamInt$new("nrounds", lower = 1L, upper = 8L, tags = "budget"),
	  ParamInt$new("max_depth", lower = 1L, upper = 10L)
	))

	task = tsk("pima")

	term = term("evals", n_evals = 100000L)
	inst = TuningInstance$new(task, lrn("classif.xgboost"), rsmp("holdout"), msr("classif.acc"), ps, term)
	tuner = tnr("hyperband", eta = 3L)

	tuner$tune(inst)

	g = autoplot(inst)
	expect_true(is.ggplot(g))	

	g = autoplot(inst, "budget")
	expect_true(is.ggplot(g))

	g = autoplot(inst, "input", params = "max_depth")
	expect_true(is.ggplot(g))
})
