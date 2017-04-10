context("rsmac")

test_that("very basic rsmac works", {
  fun = makeRosenbrockFunction(2)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  params = c("x1 real [-5,10] [0]", "x2 real [-5,10]  [0]")
  res = rsmac(fun, scenario = scenario, params = params)
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10+1)
})

test_that("rsmac wit as.pcs works", {
  fun = makeAlpine01Function(10)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  res = rsmac(fun, scenario = scenario)
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10+1)
})

