#!/usr/bin/env Rscript
status = "SUCCESS"

time.out = as.difftime(15, units = "secs")

# run id
id = sample(999999, size = 1)

cat(sprintf("Start Evaluation with id %i\n", id))

# read command line args
args = commandArgs(TRUE)
stopifnot(args[1] == "-id.smac.run")
id.smac.run = args[2]
args = tail(args, -2)
write.path = sprintf("rsmac_%s", id.smac.run)
saveRDS(object = args, file = file.path(write.path, sprintf("args_%i.rds", id)))

# wait for result file
result.file = file.path(write.path, sprintf("result_%i.rds", id))
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
file.remove(result.file)

# Output result for SMAC.
cat(sprintf("Result for SMAC: %s, %s, 0, %f, %s", status, result$runtime, result$y, result$extra))
