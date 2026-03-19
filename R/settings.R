#' @export
set_parallel_settings <-
  function(xs,
           leven = 1L,
           error = 1L,
           walkers = 1L,
           steppar = 1L) {
    checkmate::assert_int(leven)
    checkmate::assert_int(error)
    checkmate::assert_int(walkers)
    checkmate::assert_int(steppar)
    
    xspec <- xs$xspec
    reticulate::py_set_attr(xspec$Xset$parallel, "leven", leven)
    reticulate::py_set_attr(xspec$Xset$parallel, "error", error)
    reticulate::py_set_attr(xspec$Xset$parallel, "walkers", walkers)
    reticulate::py_set_attr(xspec$Xset$parallel, "steppar", steppar)
    invisible(xs)
  }

#' @export
set_fit_param_value <- function(xs, param_name, param_value) {
  checkmate::assert_choice(
    param_name,
    c(
      "query",
      "bayes",
      "criticalDelta",
      "delta",
      "method",
      "nIterations",
      "statMethod",
      "statTest"
    )
  )
  xspec <- xs$xspec
  if (param_name == "query") {
    checkmate::assert_choice(param_value, c("yes", "no", "on"))
    xspec$Fit$query <- param_value
  }
  if (param_name == "bayes") {
    checkmate::assert_choice(param_value, c("on", "off", "cons"))
    xspec$Fit$bayes <- param_value
  }
  if (param_name == "criticalDelta") {
    checkmate::assert_numeric(param_value, lower = 1e-6)
    xspec$Fit$criticalDelta <- param_value
  }
  if (param_name == "delta") {
    checkmate::assert_numeric(param_value)
    xspec$Fit$delta <- param_value
  }
  if (param_name == "method") {
    checkmate::assert_choice(param_value,
                             c('leven', 'migrad', 'minimize', 'monte', 'simplex'))
    xspec$Fit$method <- param_value
  }
  if (param_name == "nIterations") {
    checkmate::assert_int(param_value, lower = 1L)
    xspec$Fit$nIterations <- param_value
  }
  if (param_name == "statMethod") {
    checkmate::assert_choice(param_value,
                             c("chi", "cstat", "lstat", "pgstat", "pstat", "whittle"))
    xspec$Fit$statMethod <- param_value
  }
  if (param_name == "statTest") {
    checkmate::assert_choice(param_value,
                             c('ad', 'chi', 'cvm', 'ks', 'pchi', 'runs'))
    xspec$Fit$statMethod <- param_value
  }
}

#' @export
set_plot_settings <- function(xs,
                              min_sign = 0,
                              max_bins = 1L,
                              group_num = NULL,
                              add = FALSE,
                              redshift = 0) {
  checkmate::assert_number(min_sign, lower = 0)
  checkmate::assert_integer(max_bins, lower = 1)
  checkmate::assert_integer(group_num, lower = 0L, null.ok = TRUE)
  checkmate::assert_logical(add)
  checkmate::assert_number(redshift, lower = 0)
  
  xspec <- xs$xspec
  xspec$Plot$add = add
  xspec$Plot$redshift = redshift
  xspec$Plot$setRebin(min_sign, max_bins, groupNum = group_num)
  
  
  invisible(xs)
}

#' @export
reset_fit_settings <- function(x) {
  UseMethod("reset_fit_settings")
}

#' @export
reset_fit_settings.xs <- function(x) {
  xspec <- x$xspec
  xspec$Fit$bayes <- "off"
  xspec$Fit$criticalDelta <- 0.01
  xspec$Fit$delta <- 0.01
  xspec$Fit$method <- "leven"
  xspec$Fit$nIterations <- 10L
  xspec$Fit$query <- "on"
  xspec$Fit$statMethod <- "chi"
  xspec$Fit$statTest <- "chi"
  invisible(x)
}

#' @export
set_fit_settings <- function(x, ...) {
  UseMethod("set_fit_settings")
}

#' @export
set_fit_settings.xs <- function(x, ...) {
  l <- list(...)
  if (length(l) > 0) {
    purrr::walk2(names(l), l, ~ set_fit_param_value(x, .x, .y))
    update_fit_settings(x)
  }
  invisible(x)
}

#' @export
update_fit_settings <- function(x) {
  UseMethod("update_fit_settings")
}

#' @export
update_fit_settings.xs  <- function(x) {
  xspec <- x$xspec
  df <- c(
    bayes = xspec$Fit$bayes  |>  py_to_r(),
    criticalDelta = xspec$Fit$criticalDelta  |>  py_to_r(),
    delta = xspec$Fit$delta  |>  py_to_r(),
    method = xspec$Fit$method  |>  py_to_r(),
    nIterations = xspec$Fit$nIterations  |>  py_to_r(),
    query = xspec$Fit$query  |>  py_to_r(),
    statMethod = xspec$Fit$statMethod  |>  py_to_r(),
    statTest = xspec$Fit$statTest  |>  py_to_r()
    #   dof = xspec$Fit$dof  |>  py_to_r(),
    #   testStatistic = xspec$Fit$testStatistic  |>  py_to_r()
  )  |> 
    enframe(name = "param")
  class(df) <- c("xs_fit_set", class(df))
  x$fit_settings <- df
  x
}