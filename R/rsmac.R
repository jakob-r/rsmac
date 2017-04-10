#' @title Starts SMAC
#' @description Starts SMAC on the given function, with the given scenario and the given params.
#' @param fun [\code{smoof_function}]
#' @param scenario [\code{character}] \cr
#'   scenario description
#' @param params [\code{character}] \cr
#'   params in pcs format
#' @export
rsmac = function(fun, scenario, params, path.to.smac = "~/bin/smac") {
  assertClass(fun, "smoof_function")
  assertList(scenario)
  assertCharacter(params)
  path.to.smac = path.expand(path.to.smac)
  assertFileExists(file.path(path.to.smac, "smac"))

  id.smac.run = stri_paste(format(Sys.time(), "%F_%H-%M"),"_",sample(999999, 1))
  rsmac.dir = file.path(path.to.smac, sprintf("rsmac_%s", id.smac.run))
  dir.create(rsmac.dir)
  scenario.name = sprintf("rsmac-scenario-%s", id.smac.run)
  scenario.file = file.path(rsmac.dir, sprintf("%s.txt", scenario.name))

  # write scenario file
  scenario[["pcs-file"]] = file.path(rsmac.dir, "rsmac-params.pcs")
  scenario[["algo"]] = sprintf("%s -id.smac.run %s", file.path(rsmac.dir, "smac_wrapper.R"), id.smac.run)
  writeLines(
    stri_paste(names(scenario), scenario, sep = " = "),
    con = scenario.file)

  # write params file
  writeLines(params, con = file.path(rsmac.dir, "rsmac-params.pcs"))

  # write rscript to be called from smoof that wraps the
  template.file = system.file("templates/smac_wrapper.R", package = "rsmac")
  file.copy(template.file, file.path(rsmac.dir, "smac_wrapper.R"), overwrite = TRUE)
  system(sprintf("chmod +x %s", file.path(rsmac.dir, "smac_wrapper.R")))
  # Sys.chmod("./smac_wrapper.R", mode = "0755")

  # prepare OptPath
  opt.path = makeOptPathDF(par.set = getParamSet(fun), y.names = "y", minimize = shouldBeMinimized(fun), include.exec.time = TRUE)

  # start smac
  command = sprintf("(cd %s && ./smac --scenario-file %s > %s)", path.to.smac, scenario.file, file.path(rsmac.dir, "rsmac-output.txt"))
  system(command, wait=FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE)

  # check if there already was an error
  Sys.sleep(1)
  log.lines = readLines(file.path(rsmac.dir, "rsmac-output.txt"))
  err.lines = stri_detect(log.lines, regex = "ERROR")
  if (any(err.lines)) {
    stopf("There was an error in starting SMAC. \n%s\n fom log: %s", collapse(log.lines[err.lines], sep = "\n"), file.path(rsmac.dir, "rsmac-output.txt"))
  }

  smac.finished = FALSE
  iter = 1
  while (!smac.finished) {
    # 1 wait for input from rscript via file system
    catf("Waiting for new arguments from SMAC")
    args.file = character()
    while (length(args.file) == 0) {
      smac.finished = smacFinished(path.to.smac, scenario.name)
      if (smac.finished) break()
      Sys.sleep(1)
      args.file = list.files(path = rsmac.dir, pattern = "args_\\d*\\.rds", full.names = TRUE)
    }
    if (smac.finished) break()
    id = stri_extract_all(basename(args.file), regex = "(\\d+)")[[1]]
    catf("Found new arguments in file: %s", args.file)
    args = readRDS(args.file)
    args = parseArgs(args, par.set = getParamSet(fun))
    file.remove(args.file)

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
    addOptPathEl(op = opt.path, x = args, y = y, dob = iter, exec.time = res$runtime)

    # 3 write result in file (rscript will read result and return it to SMAC)
    catf("Save results in file: %s", file.path(rsmac.dir, sprintf("result_%s.rds",id)))
    saveRDS(res, file = file.path(rsmac.dir, sprintf("result_%s.rds",id)))
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
