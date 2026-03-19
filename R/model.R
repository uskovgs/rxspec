#' @export
make_model <- function(xs, model = NULL) {
  xspec <- xs$xspec
  xspec$Model(model)
  xs$model <- reticulate::py_to_r(xspec$AllModels(1L)$expression)
  update_fit_settings(xs)
  update_fit_stat(xs)
  xs$fit_params <- get_fit_params(xs, error = FALSE)
  xs
}

#' @export
load_model <- function(xs, file) {
  UseMethod("load_model")
}

#' @export
load_model.xs <- function(xs, file) {
  checkmate::assert_file_exists(file)
  xspec <- xs$xspec
  xspec$XspecSettings$restore(xspec, file)
  xs$model <- reticulate::py_to_r(xspec$AllModels(1L)$expression)
  xs$fit_params <- get_fit_params(xs, error = FALSE)
  update_fit_stat(xs)
  invisible(xs)
}


clear_model <- function(xs) {
  UseMethod("clear_model")
}
clear_model.xs <- function(xs) {
  xspec <- xs$xspec
  xspec$AllModels$clear()
  invisible(xs)
}