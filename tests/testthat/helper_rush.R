# soon replaced by import from rush package
skip_if_no_redis = function() {
  testthat::skip_on_cran()

  if (identical(Sys.getenv("RUSH_TEST_USE_REDIS"), "true") && redux::redis_available()) {
    return(invisible())
  }

  testthat::skip("Redis is not available")
}

redis_configuration = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
  config
}

start_rush = function(n_workers = 2, worker_type = "remote") {
  config = redis_configuration()

  rush::rush_plan(n_workers = n_workers, worker_type = worker_type)
  rush = rush::rsh(config = config)

  if (worker_type == "remote") {
    mirai::daemons(n_workers)
  }

  rush
}

wait_for_results = function(rush, n, timeout = 10) {
  start_time = Sys.time()
  while (rush$n_finished_tasks() < n) {
    Sys.sleep(0.1)
    if (Sys.time() - start_time > timeout) {
      stop("Timeout waiting for results")
    }
  }
}