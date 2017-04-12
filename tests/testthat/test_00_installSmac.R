context("installSmac")

test_that("smac can be downloaded and installed", {
  if (!file.exists("~/bin/smac/smac"))
    installSmac()
  expect_true(file.exists("~/bin/smac/smac"))
})
