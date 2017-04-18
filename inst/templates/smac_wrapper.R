#!/usr/bin/env Rscript

# read r environment
load("enviroment.RData")

# load packages
register = readRDS("register.rds")
for (package in register$packages) {
  library(package, character.only = TRUE)
}

status = "SUCCESS"

# find dob
dob.files = list.files(path = ".", pattern = "dob_\\d+\\.rds")
old.dob.file = tail(dob.files, 1) 
dob = readRDS(old.dob.file)

# run id
id = (sample(999999, size = 1) + as.integer(Sys.time()) + Sys.getpid()) %% 999999

cat(sprintf("Start Evaluation with id %i and dob %i\n", id, dob))

# read command line args
args = commandArgs(TRUE)

# evaluate function
fun = register$fun
args = parseArgs(args, par.set = getParamSet(fun))

start.time = Sys.time()
y = fun(args)
end.time = Sys.time()
if (hasAttributes(y, "exec.time")) {
  result$runtime = attr(y, "exec.time")
} else {
  result$runtime = as.numeric(difftime(end.time, start.time), units = "secs")
}
result$y = y
result$extra = 0

# write opt path line
args.df = do.call(cbind.data.frame, args)
x = dfRowToList(args.df, par.set = getParamSet(fun), i = 1)
op.res = list(x = x, y = y, dob = iter, exec.time = result$runtime)
writeRDS(op.res, sprintf("res_%i_%i.rds", dob, id))

# increase dob
writeRDS(dob + 1, sprintf("dob_%i.rds", dob + 1))
removeFile(old.dob.file)

# Output result for SMAC.
cat(sprintf("Result for SMAC: %s, %s, 0, %f, %s", status, result$runtime, result$y, result$extra))
