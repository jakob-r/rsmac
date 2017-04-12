context("rsmac")

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

  ncpus = if (identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "TRUE")) 8 else 2

  parallelMap::parallelStartMulticore(cpus = ncpus)
  parsmac = function(i){
    this.cl.args = c(cl.args, seed = i)
    rsmac(fn.sleep, scenario = scenario, cl.args = this.cl.args, id.smac.run = "parallel-test", cleanup = FALSE, par.id = i)
  }
  res = parallelMap::parallelMap(parsmac, i = 1:ncpus)
  parallelMap::parallelStop()
  expect_class(res[[1]], "OptPath")
  for (i in 2:ncpus) {
    expect_class(res[[i]], "OptPath")
    op1 = as.data.frame(res[[i-1]])
    op2 = as.data.frame(res[[i]])
    expect_true(nrow(op1) == 6)
    expect_true(all(op1$x1[-1] != op2$x1[-1]))
  }
})
