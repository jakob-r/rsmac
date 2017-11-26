test_that("rsmac handles trafos", {
  on.exit({
    unlink("~/bin/smac/rsmac_trafos", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-trafos", recursive = TRUE)
  })
  fun = function(x) {
    assertNumber(x$a, lower = -5, upper = 5)
    x$a^2
  }
  ps = makeParamSet(
    makeNumericParam("a", lower = -105, upper = -95, trafo = function(x) x+100)
  )
  fun = makeSingleObjectiveFunction(id = "test", fn = fun, par.set = ps, has.simple.signature = FALSE)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  res = rsmac(fun, scenario = scenario, cleanup = FALSE, id.smac.run = "trafos")
  
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10)
  x.evaluated = dfRowsToList(getOptPathX(res), getParamSet(fun))
  x.evaluated = lapply(x.evaluated, trafoValue, par = getParamSet(fun))
  y.expected = BBmisc::vnapply(x.evaluated, fun)
  expect_equal(y.expected, getOptPathY(res))
  
})

test_that("rsmac ignores trafos when pcs is passed", {
  on.exit({
    unlink("~/bin/smac/rsmac_trafos2", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-trafos2", recursive = TRUE)
  })
  fun = function(x) {
    assertNumber(x$a, lower = -5, upper = 5)
    x$a^2
  }
  ps = makeParamSet(
    makeNumericParam("a", lower = -105, upper = -95, trafo = function(x) x+100)
  )
  params = c(
    "a real [-5 , 5] [0]"
  )
  fun = makeSingleObjectiveFunction(id = "test", fn = fun, par.set = ps, has.simple.signature = FALSE)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  expect_warning({
    res = rsmac(fun, scenario = scenario, params = params, cleanup = FALSE, id.smac.run = "trafos2")  
  }, "will be ignored")
  
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10)
  y.expected = BBmisc::vnapply(dfRowsToList(getOptPathX(res), getParamSet(fun)), fun)
  expect_equal(y.expected, getOptPathY(res))
})
