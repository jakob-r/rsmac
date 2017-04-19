context("rsmac continue")

test_that("rsmac can continue", {
  on.exit({
    unlink("~/bin/smac/rsmac_continue", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-continue", recursive = TRUE)
  })
  fun = makeRosenbrockFunction(2)
  scenario = list("use-instances" = "false", runObj = "QUALITY")
  cl.args = list("shared-model-mode" = "true", "shared-model-mode-frequency" = "1")

  # first run
  init.cl.args = insert(cl.args, list("runcount-limit" = 5))
  init.res = rsmac(fun, scenario = scenario, cl.args = init.cl.args, id.smac.run = "continue", cleanup = FALSE)

  expect_equal(getOptPathLength(init.res), 5)

  # continuation run
  cont.cl.args = insert(cl.args, list("runcount-limit" = 5))
  ncpus = if (identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "TRUE")) 4 else 2

  parallelMap::parallelStartMulticore(cpus = ncpus)
  parsmac = function(i){
    this.cl.args = c(cont.cl.args, seed = i)
    rsmac(fun, scenario = scenario, cl.args = this.cl.args, id.smac.run = "continue", cleanup = FALSE, par.id = i)
  }
  res = parallelMap::parallelMap(parsmac, i = 1:ncpus)
  parallelMap::parallelStop()

  expect_class(res[[1]], "OptPath")
  op.n = sapply(res, getOptPathLength)
  expect_equal(max(op.n), ncpus * 5L + 5)
  true.res = res[[which.max(op.n)]]
  op.df = as.data.frame(true.res)
  expect_equal(nrow(unique(getOptPathX(true.res))), ncpus * 5L + 5)
})
