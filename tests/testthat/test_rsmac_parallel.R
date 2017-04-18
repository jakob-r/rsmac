context("rsmac parallel")

test_that("rsmac works in parallel", {
  on.exit({
    unlink("~/bin/smac/rsmac_parallel-test", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-parallel-test", recursive = TRUE)
  })
  fn.sleep = makeSingleObjectiveFunction(
    fn = function(x) {
      Sys.sleep(sum(x^2) + Sys.getpid()%%4)
      sum(x^2)
    },
    par.set = makeNumericParamSet(len = 2, lower = -2, upper = 2),
    has.simple.signature = TRUE)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 5)
  cl.args = list("shared-model-mode" = "true", "shared-model-mode-frequency" = "1")

  ncpus = if (identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "TRUE")) 4 else 2

  parallelMap::parallelStartMulticore(cpus = ncpus)
  parsmac = function(i){
    this.cl.args = c(cl.args, seed = i)
    rsmac(fn.sleep, scenario = scenario, cl.args = this.cl.args, id.smac.run = "parallel-test", cleanup = FALSE, par.id = i)
  }
  res = parallelMap::parallelMap(parsmac, i = 1:ncpus)
  parallelMap::parallelStop()
  expect_class(res[[1]], "OptPath")
  op.n = sapply(res, getOptPathLength)
  expect_equal(max(op.n), ncpus * 5L)
  true.res = res[[which.max(op.n)]]
  op.df = as.data.frame(true.res)
  expect_equal(nrow(unique(getOptPathX(true.res))), ncpus * 5L)
})
