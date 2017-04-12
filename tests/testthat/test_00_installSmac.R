context("installSmac")

unlink("~/bin/smac/rsmac_parallel-test", recursive = TRUE)
unlink("~/bin/smac/smac-output/rsmac-scenario-parallel-test", recursive = TRUE)
unlink("~/bin/smac/rsmac_verybasic", recursive = TRUE)
unlink("~/bin/smac/smac-output/rsmac-scenario-verybasic", recursive = TRUE)
unlink("~/bin/smac/rsmac_autopcs", recursive = TRUE)
unlink("~/bin/smac/smac-output/rsmac-scenario-autopcs", recursive = TRUE)

test_that("smac can be downloaded and installed", {
  if (!file.exists("~/bin/smac/smac"))
    installSmac()
  expect_true(file.exists("~/bin/smac/smac"))
})
