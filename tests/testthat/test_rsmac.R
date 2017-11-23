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
  expect_equal(attr(res, "par.id"), 1)
  expect_directory_exists(attr(res, "rsmac.dir"))
})

test_that("rsmac wit as.pcs works", {
  on.exit({
    unlink("~/bin/smac/rsmac_autopcs", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-autopcs", recursive = TRUE)
  })
  fun = makeAlpine01Function(10)
  scenario = list("use-instances" = "false", runObj = "QUALITY", "wallclock-limit" = 5)
  time = system.time({
    res = rsmac(fun, scenario = scenario, cleanup = FALSE, id.smac.run = "autopcs")
  })
  expect_true(time["elapsed"] > 5)
  expect_true(time["elapsed"] < 5 + 5)
  expect_class(res, "OptPath")
  expect_true(getOptPathLength(res) > 3)
})

test_that("rsmac wit as.pcs works with discretes", {
  on.exit({
    unlink("~/bin/smac/rsmac_autopcs", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-autopcs", recursive = TRUE)
  })
  fun = makeSwiler2014Function()
  scenario = list("use-instances" = "false", runObj = "QUALITY", "wallclock-limit" = 5)
  time = system.time({
    res = rsmac(fun, scenario = scenario, cleanup = FALSE, id.smac.run = "autopcs2")
  })
  expect_true(time["elapsed"] > 5)
  expect_true(time["elapsed"] < 5 + 5)
  expect_class(res, "OptPath")
  expect_true(getOptPathLength(res) > 3)
})


