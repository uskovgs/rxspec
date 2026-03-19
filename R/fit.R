

#' @export
fit_model <- function(xs) {
  xspec <- xs$xspec
  xspec$Fit$perform()
  update_fit_settings(xs)
  update_data_info(xs)
  update_fit_stat(xs)
  xs$fit_params <- get_fit_params(xs, error = FALSE)
  xs
}


update_fit_stat <- function(xs) {
  UseMethod("update_fit_stat")
}


update_fit_stat.xs <- function(xs) {
  xspec <- xs$xspec
  df <- tibble(
    dof = reticulate::py_to_r(xspec$Fit$dof),
    testStatistic = reticulate::py_to_r(xspec$Fit$testStatistic),
    statistic = reticulate::py_to_r(xspec$Fit$statistic)
  )
  class(df) <- c("xs_stat", class(df))
  xs$stat <- df
  xs
}
