writeRDS = function(object, file) {
  if (file.exists(file))
    file.remove(file)
  saveRDS(object, file = file)
  waitUntilExists(file)
  invisible(TRUE)
}

removeFile = function(file) {
  file.remove(file)
  waitUntilVanishes(file)
  invisible(TRUE)
}

waitUntilExists = function(file, negate) {
  while(!file.exists(file)) Sys.sleep(0.5)
}

waitUntilVanishes = function(file, negate) {
  while(file.exists(file)) Sys.sleep(0.5)
}

parseArgs = function(args, par.set) {
  ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  ids.vec = getParamIds(par.set, repeated = TRUE, with.nr = FALSE)
  ids.vec.unique = getParamIds(par.set, repeated = FALSE, with.nr = FALSE)
  par.types = getParamTypes(par.set, df.cols = TRUE, with.nr = TRUE, use.names = TRUE)
  arg.ids = stri_paste("-", ids)
  res = lapply(arg.ids, function(id) {
    args[which(args == id) + 1]
  })
  res = setNames(res, ids)
  for (id in ids) {
    type = par.types[id]
    if (!isTRUE(nzchar(res[[id]]))) {
      res[[id]] = NA
    }
    if (type == "numeric") {
      res[[id]] = as.numeric(res[[id]])
    } else if (type == "integer") {
      res[[id]] = as.integer(res[[id]])
    } else if (type == "factor") {
      res[[id]] = as.character(res[[id]])
    } else {
      stop("Not supported type for param id %s", id)
    }
  }

  # handle vector params
  for (id in setdiff(ids.vec, ids)) {
    id.vec.group = (ids.vec == id)
    ids.drop = ids[id.vec.group]
    res[[id]] = unlist(res[id.vec.group])
    res[ids[id.vec.group]] = NULL
  }
  # make order right
  res = res[ids.vec.unique]
  return(res)
}

