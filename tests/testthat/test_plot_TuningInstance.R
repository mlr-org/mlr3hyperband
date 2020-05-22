library(mlr3)
library(mlr3learners)
library(checkmate)
library(mlr3tuning)
library(paradox)
library(ggplot2)

context("plot_TuningInstance")

test_that("Plotting works", {
  
	ps = ParamSet$new(params = list(
	  ParamInt$new("nrounds", lower = 1L, upper = 16L, tags = "budget"),
	  ParamInt$new("max_depth", lower = 1L, upper = 10L)
	))

	task = tsk("pima")

	inst = TuningInstance$new(
		task, 
		lrn("classif.xgboost"), 
		rsmp("holdout"), 
		c(msr("classif.acc"), msr("classif.fpr")), 
		ps, 
		term("evals", n_evals = 100L)
	)

	tuner = tnr("hyperband", eta = 2L)

	tuner$tune(inst)

	g = autoplot(inst)
	expect_true(is.ggplot(g))	

	g = autoplot(inst, facet_per_bracket = FALSE)
	expect_true(is.ggplot(g))	

	g = autoplot(inst, hb_bracket = 4L)
	expect_true(is.ggplot(g))	
	g = autoplot(inst, hb_bracket = c(3L:4L))
	expect_true(is.ggplot(g))	

	g = autoplot(inst, type = "budget")
	expect_true(is.ggplot(g))

	g = autoplot(inst, type = "input", params = "max_depth")
	expect_true(is.ggplot(g))

	g = autoplot(inst, type = "input", facet_per_bracket = FALSE)
	expect_true(is.ggplot(g))

	g = autoplot(inst, measure = "classif.fpr")
	expect_true(is.ggplot(g))
})
