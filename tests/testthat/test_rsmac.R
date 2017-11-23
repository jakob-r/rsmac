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

test_that("rsmac wit as.pcs works with more param spaces", {
  on.exit({
    unlink("~/bin/smac/rsmac_autopcs", recursive = TRUE)
    unlink("~/bin/smac/smac-output/rsmac-scenario-autopcs", recursive = TRUE)
  })
  fun = function(x) {
    assertInt(x$i, lower = -1, upper = 2)
    assertChoice(x$c, c("a", "b"))
    assertNumber(x$r)
    if (x$c == "a") {
      x$r^x$i
    } else {
      sin(x$r) + cos(x$i) + 1
    }
  }
  ps = makeParamSet(
    makeDiscreteParam("c", c("a", "b")),
    makeIntegerParam("i", lower = -1, upper = 2),
    makeNumericParam("r", lower = 2^-15, upper = 3.4555e+15)
  )
  fun = makeSingleObjectiveFunction(id = "test", fn = fun, par.set = ps, has.simple.signature = FALSE)
  scenario = list("use-instances" = "false", runObj = "QUALITY", "wallclock-limit" = 5)
  time = system.time({
    res = rsmac(fun, scenario = scenario, cleanup = FALSE, id.smac.run = "autopcs2")
  })
  expect_true(time["elapsed"] > 5)
  expect_true(time["elapsed"] < 5 + 5)
  expect_class(res, "OptPath")
  expect_true(getOptPathLength(res) > 3)
})

