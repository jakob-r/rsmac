# rsmac
use smac for r functions

[![Travis-CI Build Status](https://travis-ci.org/jakob-r/rsmac.svg?branch=master)](https://travis-ci.org/jakob-r/rsmac)
[![Coverage Status](https://coveralls.io/repos/github/jakob-r/rsmac/badge.svg?branch=master)](https://coveralls.io/github/jakob-r/rsmac?branch=master)

## Installation

```r
devtools::install_github("jakob-r/rsmac")
# if smac is not installed you can run:
rsmac:::installSmac()
```

## Usage

```r
fun = makeSingleObjectiveFunction(
  name = "my_sphere",
  fn = function(x) {
    sum(x*x) + 7
  },
  par.set = makeParamSet(
    makeNumericVectorParam("x", len = 2L, lower = -5, upper = 5)
  ),
  minimize = TRUE
)
scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 10)
res = rsmac(fun, scenario = scenario)
```
To see how to set up your scenario check the [smac manual](http://www.cs.ubc.ca/labs/beta/Projects/SMAC/v2.10.03/manual.pdf).

## Notes

* This package is just tested on purely numeric functions so far.
* This package is designed for Unix systems.
* The interaction with smac works completely over the file system.
* Any help is welcome!
