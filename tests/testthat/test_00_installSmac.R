context("installSmac")

unlink("~/bin/smac/rsmac_parallel-test", recursive = TRUE)
unlink("~/bin/smac/smac-output/rsmac-scenario-parallel-test", recursive = TRUE)
unlink("~/bin/smac/rsmac_verybasic", recursive = TRUE)
unlink("~/bin/smac/smac-output/rsmac-scenario-verybasic", recursive = TRUE)
unlink("~/bin/smac/rsmac_autopcs", recursive = TRUE)
unlink("~/bin/smac/smac-output/rsmac-scenario-autopcs", recursive = TRUE)

test_that("smac can be downloaded and installed", {
  smac.path = "~/bin/smac"
  if (!file.exists(smac.path))
    installSmac(smac.path)
  expect_true(file.exists(file.path(smac.path, "smac")))
})
