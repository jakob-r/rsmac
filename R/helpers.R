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

