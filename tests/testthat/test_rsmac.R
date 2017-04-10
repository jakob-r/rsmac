context("rsmac")

test_that("very basic rsmac works", {
  fun = makeRosenbrockFunction(2)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  params = c("x1 real [-5,10] [0]", "x2 real [-5,10]  [0]")
  rsmac(fun, scenario = scenario, params = params)
})
