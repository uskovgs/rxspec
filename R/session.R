#' Create a new rxspec session
#'
#' Creates an R-side XSPEC session object and resets the current PyXspec state.
#'
#' @return An object of class \code{"xs"}.
#' @export
#' @examples
#' \dontrun{
#' specs <- Sys.glob("~/srg/PG1634+706/data/original/XMM/0852980*01/pps/*min*")
#'
#' xx <- xsession() |>
#'   set_parallel_settings(
#'     leven = 20L,
#'     error = 20L,
#'     walkers = 20L
#'   ) |>
#'   set_fit_settings(
#'     nIterations = 100L,
#'     statMethod = "cstat",
#'     query = "yes"
#'   ) |>
#'   load_data(
#'     specs = specs,
#'     grp_dat = seq_along(specs),
#'     grp_plt = seq_along(specs)
#'   ) |>
#'   ignore("bad") |>
#'   ignore_for_all("**-0.5 10.-**") |>
#'   ignore("1-3: **-0.5 10.0-**") |>
#'   make_model("const*TBabs*pow") |>
#'   set_params(2L, "0.0574 -1") |>
#'   set_params_by_name(
#'     cflux.Emin = "0.5",
#'     cflux.Emax = 3.0,
#'     zgauss_7.LineE = 30
#'   ) |>
#'   untie_params(seq(1, 36, 4)) |>
#'   link_params(5 ~ 1, 9 ~ 0.99 * 1) |>
#'   link_params(17 ~ 13, 21 ~ 13) |>
#'   link_params(33 ~ 25, 29 ~ 25) |>
#'   fit_model()
#'
#' print(xx)
#' }
xsession <- function() {
  xs <- new.env(parent = emptyenv())
  class(xs) <- "xs"

  xs$xspec <- get_xspec()

  xs$xspec$AllData$clear()
  xs$xspec$AllModels$clear()
  xs$xspec$AllChains$clear()
  xs$xspec$Xset$parallel$reset()

  xs$err_status_code <- err_status_code()
  xs$chain <- "none"
  xs$model <- "none"

  xs$files <- new_xs_files_tbl()

  xs$fit_params <- NA
  class(xs$fit_params) <- c("xs_fit_par", class(xs$fit_params))

  xs <- set_plot_settings(
    xs,
    min_sign = 0,
    max_bins = 1L,
    group_num = 1L,
    add = FALSE,
    redshift = 0
  )

  xs <- reset_fit_settings(xs)
  xs <- update_fit_settings(xs)
  xs <- update_fit_stat(xs)
  xs <- set_chain_settings(xs)

  xs
}

new_xs_files_tbl <- function() {
  tibble::tibble(
    grp_dat = integer(),
    grp_plt = integer(),
    specs = character(),
    exp = numeric(),
    rate_net = numeric(),
    rate_net_variance = numeric(),
    rate_total = numeric(),
    rate_model = numeric(),
    statistic = numeric(),
    dirs = character()
  )
}

is_irkernel <- function() {
  isTRUE(getOption("jupyter.in_kernel"))
}

display_table <- function(tbl, html = TRUE, ...) {
  if (html && is_irkernel()) {
    tbl |>
      kableExtra::kbl(booktabs = TRUE, format = "html", ...) |>
      kableExtra::kable_styling(...) |>
      as.character() |>
      IRdisplay::display_html()
  } else {
    print(tbl)
  }

  invisible(tbl)
}

#' Print an rxspec session
#'
#' @param x An object of class "xs".
#' @param ... Unused.
#' @export
print.xs <- function(
  x, ...,
  files = TRUE,
  stat = TRUE,
  fit_params = TRUE,
  model = TRUE,
  chain = TRUE
) {
  stopifnot(inherits(x, "xs"))

  ver <- reticulate::py_to_r(x$xspec$Xset$version)

  cat("<rxspec session>\n")
  cat("  PyXspec:", ver[[1]], "\n")
  cat("  XSPEC:  ", ver[[2]], "\n")
  cat("  spectra: ", nrow(x$files), "\n", sep = "")

  if (model) {
    cat("  model:   ", x$model, "\n", sep = "")
  }

  if (chain && !identical(x$chain, "none")) {
    cat("  chain:   ", x$chain, "\n", sep = "")
  }

  if (files && nrow(x$files) > 0) {
    tbl <- x$files
    if (is_irkernel()) {
      tbl |>
        kableExtra::kbl(booktabs = TRUE, format = "html") |>
        kableExtra::kable_styling(
          bootstrap_options = c("hover", "strip"),
          font_size = 12,
          full_width = TRUE
        ) |>
        as.character() |>
        IRdisplay::display_html()
    } else {
      print(tbl)
    }
  }

  if (stat && nrow(x$files) > 0) {
    tbl <- x$stat |>
      dplyr::mutate(
        pval = 1 - stats::pchisq(testStatistic, dof),
        pval = scales::pvalue(pval, 0.001)
      )

    if (is_irkernel()) {
      tbl |>
        kableExtra::kbl(booktabs = TRUE, format = "html") |>
        kableExtra::kable_styling(
          full_width = FALSE,
          font_size = 14,
          position = "float_left"
        ) |>
        as.character() |>
        IRdisplay::display_html()
    } else {
      print(tbl)
    }
  }

  if (fit_params && checkmate::test_data_frame(x$fit_params)) {
    print.xs_fit_par(x$fit_params)
  }

  invisible(x)
}