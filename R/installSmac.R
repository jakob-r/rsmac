#' @import utils
installSmac = function(dest.path = "~/bin/smac", src = "http://www.cs.ubc.ca/labs/beta/Projects/SMAC/smac-v2.10.03-master-778.tar.gz") {
  catf("I will download and copy smac to :%s", dest.path)
  tdir = tempdir()
  destfile = file.path(tdir, basename(src))
  download.file(url = src, destfile = destfile)
  # tar.file.list = untar(destfile, exdir = tdir, list = TRUE)
  untar(destfile, exdir = tdir)
  file.remove(destfile)
  dir.create(dirname(dest.path), recursive = TRUE, showWarnings = FALSE)
  system(sprintf("mv %s %s", file.path(tdir, "smac-v2.10.03-master-778"), dest.path))
  cat("SUCCESS!")
}
