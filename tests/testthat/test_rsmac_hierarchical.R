test_that("rsmac with hierarchical params", {
  on.exit({
    unlink("~/bin/smac/rsmac_hierarchical", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-hierarchical", recursive = TRUE)
  })
  fun = function(x) {
    if (x$c == "a") {
      x$a^2
    } else {
      (x$b-1)^2
    }
  }
  ps = makeParamSet(
    makeDiscreteParam("c", c("a", "b")),
    makeIntegerParam("a", lower = -3, upper = 4, requires = quote(c=="a")),
    makeNumericParam("b", lower = -5, upper = 5, requires = quote(c=="b"))
  )
  params = c(
    "c categorical { a, b } [a]",
    "a integer [-3 , 4] [0]",
    "b real [-5 , 5] [0]",
    "a | c == a",
    "b | c == b"
  )
  fun = makeSingleObjectiveFunction(id = "test", fn = fun, par.set = ps, has.simple.signature = FALSE)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  res = rsmac(fun, scenario = scenario, cleanup = FALSE, id.smac.run = "hierarchical", params = params)
  
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10)
  y.expected = BBmisc::vnapply(dfRowsToList(getOptPathX(res), getParamSet(fun)), fun)
  expect_equal(y.expected, getOptPathY(res))
  
})
