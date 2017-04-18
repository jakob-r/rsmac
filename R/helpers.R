writeRDS = function(object, file) {
  if (file.exists(file))
    file.remove(file)
  saveRDS(object, file = file)
  while(!file.exists(file)) Sys.sleep(0.5)
  invisible(TRUE)
}

removeFile = function(file) {
  file.remove(file)
  while(file.exists(file)) Sys.sleep(0.5)
  invisible(TRUE)
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

