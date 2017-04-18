#!/usr/bin/env Rscript

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

status = "SUCCESS"

time.out = as.difftime(60, units = "mins")

# run id
id = (sample(999999, size = 1) + as.integer(Sys.time()) + Sys.getpid()) %% 999999

cat(sprintf("Start Evaluation with id %i\n", id))

# read command line args
args = commandArgs(TRUE)
stopifnot(args[1] == "-id.smac.run")
id.smac.run = args[2]
args = tail(args, -2)
if (args[1] == "-par.id") {
  par.id = args[2]
  args = tail(args, -2)
} else {
  par.id = 1
}

write.path = sprintf("rsmac_%s", id.smac.run)
writeRDS(object = args, file = file.path(write.path, sprintf("args_%s_%i.rds", par.id, id)))

# wait for result file
result.file = file.path(write.path, sprintf("result_%s_%i.rds", par.id, id))
start.time = Sys.time()
cat(sprintf("Waiting to recieve result in file: %s\n", result.file))
while (!file.exists(result.file)) {
  if (difftime(Sys.time(), start.time) > time.out) {
    stop (sprintf("Timeout of %s reached", format(time.out)))
  }
  Sys.sleep(1)
}

# read result and delete file
result = readRDS(result.file)
removeFile(result.file)

# Output result for SMAC.
cat(sprintf("Result for SMAC: %s, %s, 0, %f, %s", status, result$runtime, result$y, result$extra))
