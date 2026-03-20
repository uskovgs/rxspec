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

#' @export 
plot_data <- function(df, group_var = NULL, addcomp = FALSE, legend.position="none", palette = 'Dark2', size_model = 2, size_error=1, size_pt = 1) {
    
    checkmate::assert_logical(addcomp, len = 1L)  
    
    p0 <- df |> 
        mutate(rate_lo = rate - rate_err, rate_up = rate + rate_err) |> 
        ggplot(aes(energy, model, xwidth = energy_err, color = !! enquo(group_var))) +
            xrayr::geom_xray_stepcount(aes(group = !! enquo(group_var)), size = size_model) +
            geom_linerange(
                aes(xmin = energy - energy_err, xmax = energy + energy_err, y = rate),
                linewidth = size_error,
                # color = "grey"
            ) +
            geom_linerange(
                aes(ymin = pmax(0, rate_lo), ymax = rate_up, color = !! enquo(group_var)),
                # color = "grey10",
                linewidth = size_error,
                width = 0
            ) +
            geom_point(aes(x = energy, y = pmax(0, rate), color = !! enquo(group_var)), inherit.aes = FALSE, size = size_pt) +
            ggplot2::scale_x_log10() + 
            ggplot2::scale_y_log10(
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::label_math(10^.x))
            ) +
            ggplot2::annotation_logticks() +
            ggplot2::xlab('Energy, keV') +
            ggplot2::ylab('cts/s/keV') +
            ggplot2::scale_color_brewer(palette = palette) +
            theme(legend.position = legend.position,
                legend.title = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank())
    
    
    if (addcomp) {
      n_comp <- sum(grepl("^comp", colnames(df)))
      
      if (n_comp > 1) {
        p_addcomp <- purrr::map(1:n_comp, ~ xrayr::geom_xray_stepcount(
          data = function(df) df %>% select(telescope, energy, energy_err, paste0("comp", .x)),
          mapping = ggplot2::aes_string(y = paste0("comp", .x)),
          alpha = 0.7,
          inherit.aes = TRUE
          )
        )
          
      } else {
        # cat("no additional components: ncomp =", n_comp, "\n")
        p_addcomp <- NULL
      }
    } else {
      p_addcomp <- NULL
    }
    p0 + 
      p_addcomp + 
      ggplot2::coord_cartesian(ylim = 10^(ggplot2::ggplot_build(p0)$layout$panel_params[[1]]$y.range))
}

#' @export 
plot_ratio <- function(df, group_var = NULL, palette = "Dark2", size = 1) {
    ggplot(df, aes(color = !! enquo(group_var))) +
    xrayr::geom_xray_ratio(
        aes(
            x = energy,
            xwidth = energy_err,
            obs = rate,
            obs_err = rate_err,
            model = model
        ),
        size = size
    ) +
    ggplot2::geom_hline(yintercept = 1) +
    ggplot2::scale_x_log10() +
    ggplot2::annotation_logticks(sides = 'b') +
    ggplot2::xlab('Energy, keV') +
    ggplot2::ylab('ratio') +
    ggplot2::scale_color_brewer(palette = palette) +
    theme(legend.position = "none",
          panel.grid = ggplot2::element_blank())
    
}

#' @export 
plot_all <- function(df, 
                     group_var = NULL, 
                     addcomp = FALSE, 
                     ylim_ratio = c(NA, NA), 
                     title_name = NULL, 
                     subtitle = NULL,
                     legend.position = "none",
                     palette = "Dark2",
                     rel_heights = c(3, 2),
                     size = 0.5,
                     size_model = 1, 
                     size_error = 0.25, 
                     size_pt = 1
                     ) {
    cowplot::plot_grid(
    plot_data(df, group_var = !! enquo(group_var), 
              addcomp = addcomp, 
              legend.position = legend.position,
              palette = palette, 
              size_model = size_model, 
              size_error = size_error, 
            size_pt = size_pt) + 
        theme(axis.title.x = ggplot2::element_blank(),
             axis.text.x = ggplot2::element_blank(),
             plot.title = ggplot2::element_text(size=15)
             ) +
        labs(title = title_name, subtitle = subtitle), 
    plot_ratio(df, group_var = !! enquo(group_var), palette = palette, size = size) + 
        ggplot2::coord_cartesian(ylim = ylim_ratio),
    align = "v",
    ncol = 1, 
    rel_heights = rel_heights
    )
}

#' @export 
plot_model <- function(df, ylim = range(df$model), xlim = range(df$energy), palette = "Dark2", expand = FALSE) {
  checkmate::assert_data_frame(df, min.rows = 1L, min.cols = 4L)
  
  n_comp <- df |> 
    select(starts_with("comp")) |> 
    ncol()
  
  comp_colnames <- if (n_comp > 0) paste0("comp", 1:n_comp) else  NULL
  
  model_colnames <- c("sum", comp_colnames)
  
  x_range <- range(df$energy)
  
  df |>
    dplyr::rename(sum = model) %>% 
    pivot_longer(cols = tidyr::all_of(model_colnames), names_to = "icomp", values_to = "val") %>% 
    mutate(icomp = factor(icomp, levels = model_colnames)) %>% 
    ggplot(aes(x = energy, y = val, color = icomp, linetype = icomp)) +
    geom_path() +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::annotation_logticks() +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim, expand = expand) +
    ggplot2::labs(
      x = "Energy, keV",
      y = latex2exp::TeX("Photons cm$^{-2}$ s$^{-1}$ keV$^{-1}$")
    ) +
    ggplot2::theme(
      legend.title = element_blank(),
      legend.position = c(0.9, 0.9),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
  
}