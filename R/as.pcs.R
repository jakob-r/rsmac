#' @title Converts Parameter to PCS
#' @description Coverts a ParamSet or a Param form ParamHelpers to lines of PCS definitions.
#' @param obj [\code{ParamSet|Param}]
#' @export
as.pcs = function(obj) {
  UseMethod("as.pcs")
}

#' @export
as.pcs.Param = function(obj) {
  lower = getLower(obj)
  upper = getUpper(obj)
  id = getParamIds(obj, repeated = TRUE, with.nr = TRUE)
  if (hasRequires(obj)) {
    stopf("Requirements in param %s not supported currently.", id)
  }
  type = switch(
    obj$type,
    numericvector = "real",
    discrete = "categorical",
    numeric = "real",
    integer = "integer",
    integer = "integervector")
  type = rep(type, length(id))
  default = getDefaults(obj, include.null = TRUE)
  values = getValues(obj)
  if (is.null(default)) {
    default = sampleValue(obj)
  }
  lines = character(length(id))
  for (i in seq_along(id)) {
    if (type[i] %in% c("real", "integer")) {
      lines[i] = sprintf("%s %s [%g,%g] [%g]", id[i], type[i], lower[i], upper[i], default[i])
    } else if (type[i] %in% c("categorical")) {
      lines[i] = sprintf("%s %s {%s} [%s]", id[i], type[i], stri_paste(values, collapse = ", "), default[i])
    } else {
      stopf("Not supported type %s", type[i])
    }
  }
  return(lines)
}

#' @export
as.pcs.ParamSet = function(obj){
  res = sapply(obj$pars, as.pcs)
  unlist(res)
}
