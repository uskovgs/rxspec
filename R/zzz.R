.rxspec <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .rxspec$xspec <- reticulate::import("xspec", convert = FALSE, delay_load = TRUE)

  py_path <- system.file("python", package = pkgname)
  .rxspec$x_ext <- reticulate::import_from_path(
    module = "xspec_extension",
    path = py_path,
    convert = FALSE,
    delay_load = TRUE
  )
}