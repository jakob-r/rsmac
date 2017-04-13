#' @title Starts SMAC
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
#' @param start.timeout [\code{integer(1)}] \cr
#'   For parallel usage on the same id.smac.run this helps to specify the which parallel run we are in.
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
rsmac = function(fun, scenario, params = NULL, path.to.smac = "~/bin/smac", cl.args = list(), id.smac.run = NULL, cleanup = TRUE, par.id = 1, start.timeout = 30) {
  assertClass(fun, "smoof_function")
  assertList(scenario)
  assertFlag(cleanup)

  if (is.null(params)) {
    params = as.pcs(obj = getParamSet(fun))
  }
  assertCharacter(params)
  path.to.smac = path.expand(path.to.smac)
  assertFileExists(file.path(path.to.smac, "smac"))

  # generate unqiue scenario name and sub folders
  if (is.null(id.smac.run))
    id.smac.run = stri_paste(format(Sys.time(), "%F_%H-%M"),"_",sample(999999, 1))
  assertString(id.smac.run)
  rsmac.dir = file.path(path.to.smac, sprintf("rsmac_%s", id.smac.run))
  dir.create(rsmac.dir)
  scenario.name = sprintf("rsmac-scenario-%s", id.smac.run)
  scenario.file = file.path(rsmac.dir, sprintf("%s.txt", scenario.name))

  # write scenario file
  default.scenario = list(
    "pcs-file" = file.path(rsmac.dir, "rsmac-params.pcs")
  )
  scenario = insert(default.scenario, scenario)
  writeLines(
    stri_paste(names(scenario), scenario, sep = " = "),
    con = scenario.file)

  # deal with CL args
  default.cl.args = list(
    "scenario-file" = scenario.file,
    "algo-exec" = sprintf("'%s -id.smac.run %s -par.id %i'", file.path(rsmac.dir, "smac_wrapper.R"), id.smac.run, par.id),
    "initial-incumbent" = "RANDOM"
  )
  cl.args = insert(default.cl.args, cl.args)
  assertList(cl.args, min.len = 1L)

  # write params file
  writeLines(params, con = file.path(rsmac.dir, "rsmac-params.pcs"))

  # write rscript to be called from smoof that wraps the
  template.file = system.file("templates/smac_wrapper.R", package = "rsmac")
  file.copy(template.file, file.path(rsmac.dir, "smac_wrapper.R"), overwrite = TRUE)
  system(sprintf("chmod +x %s", file.path(rsmac.dir, "smac_wrapper.R")))
  # Sys.chmod("./smac_wrapper.R", mode = "0755")

  # take care of cleanup
  if (cleanup) {
    on.exit({
      unlink(rsmac.dir, recursive = TRUE)
      unlink(file.path(path.to.smac, "smac-output", scenario.name), recursive = TRUE)
    }, add = TRUE)
  }

  # prepare OptPath
  opt.path = makeOptPathDF(par.set = getParamSet(fun), y.names = "y", minimize = shouldBeMinimized(fun), include.exec.time = TRUE)

  # start smac
  cl.output.file = file.path(rsmac.dir, sprintf("rsmac-output-%i.txt", par.id))
  command = sprintf(
    "(cd %s && ./smac --%s > %s)",
    path.to.smac,
    stri_paste(names(cl.args), cl.args, collapse = " --", sep = " "),
    cl.output.file)
  system(command, wait=FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE)
  # check if there already was an error
  Sys.sleep(1)
  log.lines = readLines(cl.output.file)
  err.lines = stri_detect(log.lines, regex = "ERROR")
  if (any(err.lines)) {
    stopf("There was an error in starting SMAC. \n%s\n fom log: %s", collapse(log.lines[err.lines], sep = "\n"), file.path(rsmac.dir, "rsmac-output.txt"))
  }

  smac.finished = FALSE
  already.read.args.files = character()
  iter = 1
  while (!smac.finished) {
    # 1 wait for input from rscript via file system
    catf("Waiting for new arguments from SMAC")
    args.file = character()
    start.time = Sys.time()
    while (length(args.file) == 0) {
      if (cleanup) {
        smac.finished = smacFinished(path.to.smac, scenario.name)
      } else {
        smac.finished = (difftime(Sys.time(), start.time) > as.difftime(start.timeout, units = "secs"))
        if (smac.finished && length(already.read.args.files)==0) {
          stop("Timeout while waiting for args.file.")
        }
      }
      if (smac.finished) break()
      Sys.sleep(1)
      args.file = list.files(
        path = rsmac.dir,
        pattern = sprintf("args_%i_\\d*\\.rds", par.id),
        full.names = TRUE)
      args.file = setdiff(args.file, already.read.args.files) # prevents unfortunaltely not deleted files from beein read again
    }
    if (smac.finished) break()
    args.file = args.file[Sys.getpid() %% length(args.file) + 1]
    id = stri_extract_all(
      basename(args.file),
      regex = sprintf("(?<=args_%i_)\\d+(?=\\.rds)", par.id))[[1]]
    catf("Found new arguments in file: %s", args.file)
    args = readRDS(args.file)
    already.read.args.files = c(already.read.args.files, args.file)
    args = parseArgs(args, par.set = getParamSet(fun))
    removeFile(args.file)

    res = list(runtime = NULL, y = NULL, extra = NULL)

    # 2 call function with params from input
    start.time = Sys.time()
    y = fun(args)
    end.time = Sys.time()
    if (hasAttributes(y, "exec.time")) {
      res$runtime = attr(y, "exec.time")
    } else {
      res$runtime = as.numeric(difftime(end.time, start.time), units = "secs")
    }
    assertNumber(y)
    res$y = y
    res$extra = 0

    # add things to opt path
    args.df = do.call(cbind.data.frame, args)
    x = dfRowToList(args.df, par.set = getParamSet(fun), i = 1)
    addOptPathEl(op = opt.path, x = x, y = y, dob = iter, exec.time = res$runtime)

    # 3 write result in file (rscript will read result and return it to SMAC)
    result.file = sprintf("result_%i_%s.rds", par.id, id)
    catf("Save results in file: %s", file.path(rsmac.dir, result.file))
    writeRDS(res, file = file.path(rsmac.dir, result.file))
    iter = iter + 1
  }
  return(opt.path)
}

smacFinished = function(path.to.smac, scenario.name) {
  files = list.files(file.path(path.to.smac, "smac-output", scenario.name), pattern = "validationResults.*\\.csv")
  length(files) == 1
}

parseArgs = function(args, par.set) {
  ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  par.types = getParamTypes(par.set, df.cols = TRUE, with.nr = TRUE, use.names = TRUE)
  arg.ids = stri_paste("-", ids)
  res = lapply(arg.ids, function(id) {
    args[which(args == id) + 1]
  })
  res = setNames(res, ids)
  for (id in ids) {
    type = par.types[id]
    if (type == "numeric") {
      res[[id]] = as.numeric(res[[id]])
    }
  }
  return(res)
}
