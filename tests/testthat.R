Sys.setenv("R_TESTS" = "")
library(testthat)
library(rsmac)

test_check("rsmac")
