context("rsmac")

test_that("rsmac works in parallel", {
  fn.sleep = makeSingleObjectiveFunction(
    fn = function(x) {
      Sys.sleep(sum(x^2) + Sys.getpid()%%4)
      sum(x^2)
    },
    par.set = makeNumericParamSet(len = 2, lower = -2, upper = 2),
    has.simple.signature = TRUE)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 20)
  cl.args = list("shared-model-mode" = "true", "shared-model-mode-frequency" = "1")

  parallelMap::parallelStartMulticore(cpus = 2)
  parsmac = function(seed){
    this.cl.args = c(cl.args, seed = seed)
    rsmac(fn.sleep, scenario = scenario, cl.args = this.cl.args, id.smac.run = "parallel-test", cleanup = FALSE)
  }
  res = parallelMap::parallelMap(parsmac, 1:2)
  parallelMap::parallelStop()
  unlink("~/bin/smac/rsmac_parallel-test", recursive = TRUE)
  unlink("~/bin/smac/smac-output/rsmac-scenario-parallel-test", recursive = TRUE)
  expect_class(res[[1]], "OptPath")
  expect_class(res[[2]], "OptPath")
})
