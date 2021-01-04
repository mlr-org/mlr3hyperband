library(mlr3learners)
library(mlr3pipelines)
library(checkmate)

context("TunerSuccessiveHalving")


test_that("TunerHyperband singlecrit", {

	# Remove later
	mlr_tuners$add("sh", TunerSuccessiveHalving)	

	par.set = ParamSet$new(params = list(
		ParamInt$new("nrounds", lower = 20, upper = 2^8,
		  tags = "budget"),
		ParamInt$new("max_depth", lower = 1, upper = 100)
	))
	task = tsk("pima")

	learner = lrn("classif.xgboost")

	term = trm("none")

    measures = c("classif.tpr", "classif.fpr")

    inst = TuningInstanceMultiCrit$new(task = task, learner = learner,
      resampling =  rsmp("holdout"), measure = msrs(measures), terminator = term,
      search_space = par.set)

    tuner = mlr3tuning::tnr("sh", eta = 2, n = 2^5, r = 20, mo_method = "indicator_based", np = 2, tie_breaker = "CD")

    tuner$optimize(inst)


    inst = TuningInstanceMultiCrit$new(task = task, learner = learner,
      resampling =  rsmp("holdout"), measure = msrs(measures), terminator = term,
      search_space = par.set)

    tuner = mlr3tuning::tnr("sh", eta = 2, n = 2^5, r = 20, mo_method = "dominance_based", np = 1, tie_breaker = "CD")

    tuner$optimize(inst)
})



test_that("TunerSuccessiveHalving synthetic", {

	mlr_tuners$add("sh", TunerSuccessiveHalving)	

	# Define objective function
	fun = function(xs) {
		# This is a simple multi-objective function
		# We sample a noise within a specific radius
		# The higher the budget, the smaller the noise
		radius = 1 / xs[[3]]
		phi = runif(1, 0, 2 * pi)
		noise = c(radius * cos(phi), radius * sin(phi)) 
		fun = c(xs[[1]]^2, - xs[[1]] + 3 + xs[[2]])
		res = fun + noise
	}

	# Set domain
	domain = ParamSet$new(list(
		ParamDbl$new("x1", 0, 3),
		ParamDbl$new("x2", 0, 4),
		ParamDbl$new("x3", 2, 8, tag = "budget")
	))

	# Set codomain
	codomain = ParamSet$new(list(
	  ParamDbl$new("y1", tags = "minimize"),
	  ParamDbl$new("y2", tags = "minimize")
	))

	obj = ObjectiveRFun$new(
			  fun = fun,
			  domain = domain,
			  codomain = codomain, 
			  properties = "noisy",
			  check_values = FALSE
			) 

	# Define termination criterion
	terminator = trm("none")

	# Create optimization instance
	inst = OptimInstanceMultiCrit$new(
	  objective = obj, 
	  search_space = domain,
	  terminator = terminator
	  )

    tuner = mlr3tuning::tnr("sh", eta = 2, n = 2^5, r = 2, mo_method = "indicator_based", np = 5)

    tuner$optimize(inst)
})