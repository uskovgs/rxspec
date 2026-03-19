#' @export
get_plot_data <- function(xs, plot = "data", x = "keV", addcomp = FALSE, n_plt_grp = NULL) {
  UseMethod("get_plot_data")
}

#' @export
get_plot_data.xs <- function(xs, plot = "data", x = "keV", addcomp = FALSE, n_plt_grp = NULL) {
  checkmate::assert_choice(plot, c("data", 
                                   "model", 
                                   "emodel",
                                   "eemodel",
                                   "ufspec", 
                                   "eufspec", 
                                   "eeufspec"))
  checkmate::assert_choice(x,
                           c(
                             "channel",
                             "keV",
                             "MeV",
                             "GeV",
                             "Hz",
                             "angstrom",
                             "cm",
                             "micron"
                           ))
  
  xspec <- xs$xspec
  n_spec <- reticulate::py_to_r(xspec$AllData$nSpectra)
  checkmate::assert_logical(addcomp, len = 1L)
  checkmate::assert_number(n_plt_grp, lower = 1, null.ok = TRUE)
  
  
  
  xspec$Plot$device = "/null"
  xspec$Plot$add = addcomp
  
  
  xspec$Plot$xAxis = x
  xspec$Plot(plot)
  
  if (is.null(n_plt_grp) && n_spec != 0) n_plt_grp <- n_spec else n_plt_grp <- 1L
  
  comp_names <- reticulate::py_to_r(xspec$AllModels(1L)$componentNames) |>  
    stringi::stri_extract(regex = "[^_\\d]+")
  
  n_comp <- sum(comp_names %in% additive_components())
  print(n_comp)
  is_model_exist = FALSE
  try({
    xspec$AllModels(1L)
    is_model_exist = TRUE
  },
  silent = TRUE
  )

  
  1:n_plt_grp |> 
    purrr::map(
      \(i) {
        
        
        if (plot %in% c("data", "eufspec", "eeufspec")) {
          rate1 <- reticulate::py_to_r(xspec$Plot$y(i))
          rate_err1 <- reticulate::py_to_r(xspec$Plot$yErr(i))
        } else {
          rate1 <- NULL
          rate_err1 <- NULL
        }
        
        df <- tibble(
          telescope = i,
          energy = reticulate::py_to_r(xspec$Plot$x(i)),
          energy_err = reticulate::py_to_r(xspec$Plot$xErr(i)),
          rate = rate1,
          rate_err = rate_err1
        )
        
        
        if(is_model_exist) {
          df$model = reticulate::py_to_r(xspec$Plot$model(i))
          
          if (addcomp && n_comp > 1) {
            for (comp in seq(n_comp)) {
              df[[paste0("comp", comp)]] <- reticulate::py_to_r(
                xspec$Plot$addComp(addCompNum = comp, plotGroup = i)
              )
            }
          }
        }
       df 
      } 
    )  |> 
    purrr::list_rbind() |>
    mutate(telescope = as.character(telescope))
}


#' @export
plot_steppar2d <- function(out_steppar2d, delstat = c(0.68, 0.9, 0.99), show_min_stat = FALSE, ...) { 
  UseMethod("plot_steppar2d")
}

#' @export
plot_steppar2d.xs_steppar2d <- function(out_steppar2d, delstat = c(0.68, 0.9, 0.99), show_min_stat = FALSE, ...) {

  checkmate::assert_numeric(delstat, null.ok = FALSE, lower = 0, upper = 1)

  min_stat <- min(out_steppar2d$stat)
  xmin <- out_steppar2d$val1[which.min(out_steppar2d$stat)]
  ymin <- out_steppar2d$val2[which.min(out_steppar2d$stat)]
  df_stat_min <- tibble(x = xmin, y = ymin, stat = min_stat)

  delstat_labels <- paste0(round(delstat * 100, 2), "%")
  
  p <- out_steppar2d |>
    ggplot2::ggplot(ggplot2::aes(val1, val2)) +
    ggplot2::geom_contour(aes(z = stat, color = factor(ggplot2::after_stat(level)), linetype = factor(ggplot2::after_stat(level))),
      breaks = min_stat + qchisq(delstat, 2),

    ) +
    ggplot2::geom_point(
      data = df_stat_min,
      mapping = ggplot2::aes(x, y),
      size = 2
    ) +
    ggplot2::scale_linetype("Conf. levels", labels = delstat_labels) +
    ggplot2::scale_color_discrete("Conf. levels", labels = delstat_labels)

    if (show_min_stat)
      p <- p + ggplot2::geom_text(
        data = df_stat_min,
        mapping = ggplot2::aes(x, y, label = round(min_stat, 2)),
        ...
      )

      return(p)
}

#' @export
plot_steppar.xs_steppar <- function(out_steppar, delstat = 1) {
    min_stat <- min(out_steppar$stat)
    val_with_min_stat <- out_steppar$val[which(out_steppar$stat == min_stat)]

    val_at_delstat <- calc_error_from_steppar(out_steppar, delstat)

    out_steppar |> 
        ggplot2::ggplot(ggplot2::aes(val, stat)) +
        ggplot2::geom_path() +
        ggplot2::geom_hline(yintercept = min_stat + c(0, delstat)) +
        ggplot2::geom_vline(xintercept = val_with_min_stat, linetype = "longdash") +
        ggplot2::labs(
            title = paste0("Min Stat = ", round(min_stat, 2)),
            subtitle = paste0(
              "Val with min stat = ", val_with_min_stat, ". ",
              "Val at delta stat = ", val_at_delstat
            )
        )
}