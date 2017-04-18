context("rsmac")

test_that("very basic rsmac works", {
  on.exit({
    unlink("~/bin/smac/rsmac_verybasic", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-verybasic", recursive = TRUE)
  })
  fun = makeRosenbrockFunction(2)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  params = c("x1 real [-5,10] [1]", "x2 real [-5,10]  [1]")
  res = rsmac(fun, scenario = scenario, params = params, cleanup = FALSE, id.smac.run = "verybasic")
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10)
  expect_equal(nrow(unique(getOptPathX(res))), 10)
  expect_equal(apply(getOptPathX(res), 1, fun), getOptPathY(res))
})

test_that("rsmac wit as.pcs works", {
  on.exit({
    unlink("~/bin/smac/rsmac_autopcs", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-autopcs", recursive = TRUE)
  })
  fun = makeAlpine01Function(10)
  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
  res = rsmac(fun, scenario = scenario, cleanup = FALSE, id.smac.run = "autopcs")
  expect_class(res, "OptPath")
  expect_equal(getOptPathLength(res), 10)
})

