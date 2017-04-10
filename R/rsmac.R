#' @title Starts SMAC
#' @description Starts SMAC on the given function, with the given scenario and the given params.
#' @param fun [\code{smoof_function}]
#' @param scenario [\code{character}] \cr
#'   scenario description
#' @param params [\code{character}] \cr
#'   params in pcs format
#' @export
rsmac = function(fun, scenario, params) {
  assertClass(fun, "smoof_function")
  assertCharacter(scenario)
  assertCharacter(params)

  # write scenario file

  # write params file

  # write rscript to be called from smoof that wraps the

  # start smac
  system("./smac --scenario-file ./rsmac-scenario.txt", wait=FALSE)

  # 1 wait for input from rscript via file system

  # 2 call function with params from input

  # 3 write result in file (rscript will read result and return it to SMAC)

  # go to 1


}
