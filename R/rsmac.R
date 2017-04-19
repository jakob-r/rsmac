#' @title Starts SMAC (alternative)
#' @description Starts SMAC on the given function, with the given scenario and the given params.
#' @param fun [\code{smoof_function}]
#' @param scenario [\code{list}] \cr
#'   scenario description
#' @param params [\code{character}] \cr
#'   Params in pcs format. Default is \code{\link{as.pcs}(getParamSet(fun))}.
#' @param path.to.smac [\code{character(1)}] \cr
#'   The directory where the smac binary is located.
#'   All intermediate files will be saved there.
#' @param cl.args [\code{list}] \cr
#'   CL arguments that should be passed when calling \code{./smac}.
#' @param id.smac.run [\code{character(1)}] \cr
#'   You might want to overrride the automaticly generated ID to do restarts or parallel optimizations on the exact same scenario.
#' @param cleanup [\code{logical(1)}] \cr
#'   Should all files be deleted after the run?
#' @param par.id [\code{integer(1)}] \cr
#'   For parallel usage on the same id.smac.run this helps to specify the which parallel run we are in.
#'   Only the call with \code{par.id = 1} will write the enviroment to the disk.
#' @examples
#'  \dontrun{
#'  scenario = list("use-instances" = "false", runObj = "QUALITY", numberOfRunsLimit = 5)
#'  res = rsmac(makeBraninFunction(), scenario = scenario)
#'  best.idx = getOptPathBestIndex(res)
#'  getOptPathEl(res, best.idx)
#'  as.data.frame(res)
#'  }
#' @return \link[ParamHelpers]{OptPath}
#' @export
rsmac = function(fun, scenario, params = NULL, path.to.smac = "~/bin/smac", cl.args = list(), id.smac.run = NULL, cleanup = TRUE, par.id = 1) {
  assertClass(fun, "smoof_function")
  assertList(scenario)
  assertFlag(cleanup)

  if (par.id > 1 && is.null(id.smac.run)) {
    stop("For a par.id > 1 you have to supply a id.smac.run!")
  }

  if (par.id > 1 && cleanup) {
    stop("Parallel runs can not run with automatic cleanup!s")
  }

  if (is.null(params)) {
    params = as.pcs(obj = getParamSet(fun))
  }
  assertCharacter(params)
  path.to.smac = path.expand(path.to.smac)
  assertFileExists(file.path(path.to.smac, "smac"))

  # generate unqiue scenario name and sub folders
  if (is.null(id.smac.run)) {
    id.smac.run = stri_paste(
      format(Sys.time(), "%F_%H-%M"),
      "_",
      (sample(999999, size = 1) + as.integer(Sys.time()) + Sys.getpid()) %% 999999)
  }
  assertString(id.smac.run)
  rsmac.dir = file.path(path.to.smac, sprintf("rsmac_%s", id.smac.run))
  if (par.id > 1) waitUntilExists(rsmac.dir) else dir.create(rsmac.dir)
  scenario.name = sprintf("rsmac-scenario-%s", id.smac.run)
  scenario.file = file.path(rsmac.dir, sprintf("%s.txt", scenario.name))

  # write scenario file
  default.scenario = list(
    "pcs-file" = file.path(rsmac.dir, "rsmac-params.pcs"),
    "algo" = sprintf("%s -id.smac.run %s", file.path(rsmac.dir, "smac_wrapper.R"), id.smac.run)
  )
  scenario = insert(default.scenario, scenario)
  if (par.id > 1)
    waitUntilExists(scenario.file)
  else {
    writeLines(
      stri_paste(names(scenario), scenario, sep = " = "),
      con = scenario.file)
  }

  # deal with CL args
  default.cl.args = list(
    "scenario-file" = scenario.file,
    "initial-incumbent" = "RANDOM",
    "num-validation-runs" = 0
  )
  cl.args = insert(default.cl.args, cl.args)
  assertList(cl.args, min.len = 1L)

  # write params file
  params.file = file.path(rsmac.dir, "rsmac-params.pcs")
  if (par.id > 1) waitUntilExists(params.file) else writeLines(params, con = params.file)

  # write rscript to be called from smoof that wraps the
  template.file = system.file("templates/smac_wrapper.R", package = "rsmac")
  script.file = file.path(rsmac.dir, "smac_wrapper.R")
  if (par.id > 1)
    waitUntilExists(script.file)
  else {
    file.copy(template.file, script.file, overwrite = TRUE)
    system(sprintf("chmod +x %s", file.path(rsmac.dir, "smac_wrapper.R")))
  }

  # write enviroment for the smac_wrapper
  enviroment.file = file.path(rsmac.dir, "enviroment.RData")
  if (par.id > 1) waitUntilExists(enviroment.file) else save.image(enviroment.file)

  # write register for the smac_wrapper
  register = list(
    packages = names(sessionInfo()$otherPkgs),
    fun = fun)
  register.file = file.path(rsmac.dir, "register.rds")
  if (par.id > 1) waitUntilExists(register.file) else writeRDS(register, register.file)

  # write initial dob file
  init.dob.file = file.path(rsmac.dir, sprintf("dob_%.6i.rds", 0))
  if (par.id > 1) waitUntilExists(init.dob.file) else writeRDS(0, init.dob.file)


  # take care of cleanup
  if (cleanup) {
    on.exit({
      unlink(rsmac.dir, recursive = TRUE)
      unlink(file.path(path.to.smac, "smac-output", scenario.name), recursive = TRUE)
    }, add = TRUE)
  }

  # start smac
  cl.output.file = file.path(rsmac.dir, sprintf("rsmac-output-%i.txt", par.id))
  command = sprintf(
    "(cd %s && ./smac --%s > %s)",
    path.to.smac,
    stri_paste(names(cl.args), cl.args, collapse = " --", sep = " "),
    cl.output.file)
  system(command, wait = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE, intern = FALSE)

  # Write OptPath
  opt.path = makeOptPathDF(par.set = getParamSet(fun), y.names = "y", minimize = shouldBeMinimized(fun), include.exec.time = TRUE)

  opt.el.files = list.files(
        path = rsmac.dir,
        pattern = sprintf("res_\\d+_\\d+\\.rds"),
        full.names = TRUE)
  opt.els = lapply(opt.el.files, readRDS)
  for (opt.el in opt.els) {
    addOptPathEl(op = opt.path, x = opt.el$x, y = opt.el$y, dob = opt.el$dob, exec.time = opt.el$exec.time)
  }
  res = opt.path
  attr(res, "rsmac.dir") = rsmac.dir
  attr(res, "par.id") = par.id
  return(res)
}
