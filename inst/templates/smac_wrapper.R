#!/usr/bin/env Rscript

# read command line args
args = commandArgs(TRUE)
stopifnot(args[1] == "-id.smac.run")
id.smac.run = args[2]
args = tail(args, -2)

write.path = sprintf("rsmac_%s", id.smac.run)


# read r environment
load(file.path(write.path, "enviroment.RData"))

# load packages
register = readRDS(file.path(write.path, "register.rds"))
for (package in register$packages) {
  library(package, character.only = TRUE)
}

status = "SUCCESS"

# find dob
dob.files = list.files(path = write.path, pattern = "dob_\\d+\\.rds")
old.dob.file = tail(dob.files, 1)
dob = readRDS(file.path(write.path, old.dob.file))

# run id
id = (sample(999999, size = 1) + as.integer(Sys.time()) + Sys.getpid()) %% 999999

cat(sprintf("Start Evaluation with id %i and dob %i\n", id, dob))

# evaluate function
fun = register$fun
args = rsmac:::parseArgs(args, par.set = getParamSet(fun))

start.time = Sys.time()
y = fun(args)
end.time = Sys.time()
if (hasAttributes(y, "exec.time")) {
  runtime = attr(y, "exec.time")
} else {
  runtime = as.numeric(difftime(end.time, start.time), units = "secs")
}
y = y
extra = 0

# write opt path line
args.df = do.call(cbind.data.frame, args)
x = dfRowToList(args.df, par.set = getParamSet(fun), i = 1)
op.res = list(x = x, y = y, dob = dob, exec.time = runtime)
rsmac:::writeRDS(op.res, file.path(write.path, sprintf("res_%.6i_%i.rds", dob, id)))

# increase dob
rsmac:::writeRDS(dob + 1, file.path(write.path, sprintf("dob_%.6i.rds", dob + 1)))
rsmac:::removeFile(file.path(write.path, old.dob.file))

# Output result for SMAC.
cat(sprintf("Result for SMAC: %s, %s, 0, %f, %s", status, runtime, y, extra))
