#' @export
get_fit_params <- function(xs, error = FALSE, level = 90) {
  UseMethod("get_fit_params")
}

#' @export
get_fit_params.xs <- function(xs, error = FALSE, level = 90) {
  xspec <- xs$xspec
  n_groups <- reticulate::py_to_r(xspec$AllData$nGroups)
  
  assertthat::assert_that(level < 100 && level > 0)
  
  df <- purrr::map(1:n_groups, \(ii) get_fit_params_i(xs, ii, error = error, level = level)) |> 
    purrr::list_rbind(names_to = "grp_dat") |>             
    mutate(i = 1:n())  |> 
    select(grp_dat, i, everything())

  if (error) {

    df1 <- purrr::map(1:n_groups, \(ii) get_fit_params_i(xs, ii, error = FALSE, level = level)) |> 
      purrr::list_rbind(names_to = "grp_dat") |> 
      mutate(i = 1:n()) |> 
      select(grp_dat, i, everything())

    df <- df |> 
      modifyList(df1)
  }

  

  class(df) <- c("xs_fit_par", class(df))

  update_fit_settings(xs)
  update_data_info(xs)
  update_fit_stat(xs)
  xs$fit_params <- df
  
  df
}


get_fit_params_i <- function(xs, model_grp, error = FALSE, level = 90) {
  xspec <- xs$xspec
  m <- xspec$AllModels(model_grp)
  q_chi <- qchisq(level / 100, df = 1) |> 
    round(3) |> 
    sprintf("%.3f", ... = _)
  
  # rewrite
  
  df <- m$componentNames |> 
    py_to_r() |> 
    tibble(comp = _) |> 
    mutate(param = purrr::map(comp, ~ eval(parse(
      text = glue("m${.x}$parameterNames")
    ))  |> 
      py_to_r())) |> 
    tidyr::unnest_longer(param)  |> 
    mutate(
      unit = purrr::map2_chr(comp, param,
                      ~ eval(parse(
                        text = glue("m${.x}${.y}$unit")
                      ))  |> 
                        py_to_r()),
      values = purrr::map2(comp, param,
                    ~ eval(parse(
                      text = glue("m${.x}${.y}$values")
                    ))  |> 
                      py_to_r()  |> 
                      purrr::set_names(
                        c("val", "delta", "min", "bot", "top", "max")
                      )),
      frozen = purrr::map2_lgl(comp, param,
                        ~ eval(parse(
                          text = glue("m${.x}${.y}$frozen")
                        )) |> 
                          py_to_r()),
      frozen = ifelse(frozen == TRUE, "+", ""),
      link = purrr::map2_chr(comp, param,
                      ~ eval(parse(
                        text = glue("m${.x}${.y}$link")
                      ))  |> 
                        py_to_r())
    )  |> 
    tidyr::unnest_wider(values)  |> 
    select(comp, param, unit, frozen, link, val, delta, min, bot, top, max)
  
  
  # recalc it twice
  
  if (error) {
    i_free <-
      nrow(df) * (model_grp - 1) + which(df$frozen == "" & df$link == "")
    
    print(i_free)
    purrr::walk(i_free, ~ xspec$Fit$error(paste(q_chi, as.character(.x))))
    df <- df  |> 
      mutate(err = purrr::map2(
        comp,
        param,
        ~ glue("m${.x}${.y}$error")  |> 
          parse(text = _)  |> 
          eval()  |> 
          py_to_r()  |> 
          purrr::set_names(c(
            "lower", "upper", "err_status_code"
          ))
      )) |> 
      tidyr::unnest_wider(err) |> 
      mutate(
        ii = purrr::map(
          err_status_code,
          ~ gsub("(.{1})",         # Apply gsub
                 "\\1,",
                 .x) %>%
            gsub(",$", "", .) %>%
            paste0("c(", ., ")")  |> 
            parse(text = _)  |> 
            eval()
        ),
        decoding = purrr::map_chr(ii, ~ paste(err_status_code()[.x],
                                       collapse = ",\n")),
        
      )  |> 
      select(-all_of(c("val", "delta", "min", "bot", "top", "max"))) |> 
      mutate(values = purrr::map2(comp, param,
                           ~ eval(parse(
                             text = glue("m${.x}${.y}$values")
                           ))  |> 
                             py_to_r() |> 
                             purrr::set_names(
                               c("val", "delta", "min", "bot", "top", "max")
                             )))  |> 
      tidyr::unnest_wider(values) |> 
      select(-ii) |> 
      select(comp, param, unit, frozen, link, val, lower, upper, delta, min, bot, top, max,  err_status_code, decoding)

    
    xs <- update_fit_stat.xs(xs)
  }

  df
}

#' @export
set_params <- function(xs, id = NULL, val = NULL) {
  if (!is.null(id) && !is.null(val)) {
    df <- get_fit_params(xs, error = FALSE)
    checkmate::assert_int(id, lower = 1, upper = max(df$i))
    id <- as.integer(id)
    
    model_id <- df[df$i == id,]$grp_dat[[1]]  |>  as.integer()
    m <- xs$xspec$AllModels(model_id)
    
    df_at_model_id <- df[df$grp_dat == model_id,]
    param_index <- which(df_at_model_id$i == id)  |>  as.character()
    l <- list()
    l[[param_index]] <- val
    m$setPars(reticulate::dict(l))
    xs$fit_params <- get_fit_params(xs, error = FALSE)
  }
  invisible(xs)
}

#' @export
set_params_by_name <- function(xs, ...) {
  l <- list(...)  |> unlist()
  if (length(l) > 0) {
    xspec <- xs$xspec
    m <- xspec$AllModels(1L)
    
    splitted_names <- stringi::stri_split(names(l), regex = "[\\.\\$]")
    comp_names <- purrr::map_chr(splitted_names, ~ as.character(.x[1]))
    param_names <- purrr::map_chr(splitted_names, ~ as.character(.x[2]))
    comp_param_names <- paste(comp_names, param_names, sep = ".")
    
    df <- get_fit_params_i(xs, 1L, error = FALSE)  |> 
      mutate(id = 1:n())
    
    df1 <- df  |> 
      mutate(comp_param = paste(comp, param, sep="."))  |> 
      filter(comp_param %in% comp_param_names)
    
    
    checkmate::assert_set_equal(length(l), nrow(df1))
    
    
    id <- purrr::map2_chr(
      comp_names, 
      param_names, 
      ~ filter(df, comp == .x, param == .y) |> 
        _$id |>
        as.character()
    )
    
    ll <- list()
    
    for (i in 1:length(id)) {
      ll[[ id[i] ]] <- l[i]
    }
    

    m$setPars(reticulate::dict(ll))
  }
  xs$fit_params <- get_fit_params(xs, error = FALSE)
  
  invisible(xs)
}

#' @export
link_params <- function(xs, ...) {
  UseMethod("link_params")
}

#' @export
link_params.xs <- function(xs, ...) {
  # Example: link_params.xs(xs, 4~2, 5 ~ 0.5 * 3)
  
  l <- list(...)
  if (length(l) > 0) {
    checkmate::assert_formula(l[[1]])
    df <- get_fit_params(xs, error = FALSE)
    left <- formula.tools::lhs(l)
    right <- formula.tools::rhs(l)
    
    purrr::walk2(left, right, function(ll, rr) {
      id <- as.integer(ll)
      model_id <- df[df$i == id,]$grp_dat[[1]]  |> as.integer()
      df_at_model_id <- df[df$grp_dat == model_id,]
      param_index <- which(df_at_model_id$i == id)
      m <- xs$xspec$AllModels(model_id)
      
      rr <- as.character(rr)
      if (length(rr) > 1)
        rr <- paste(rr[c(2, 1, 3)], collapse = "")
      param <- xext_model_comp_id(m, param_index)
      param$link <- rr
    })

    xs$fit_params <- get_fit_params.xs(xs)
    xs <- update_fit_stat(xs)

  }
  invisible(xs)
}

#' @export
untie_params <- function(xs, ...) {
  UseMethod("untie_params")
}

#' @export
untie_params.xs <- function(xs, ...) {
  # Example: untie_params(xs, 4, 5)
  l <- list(...) %>% unlist()
  if (length(l) > 0) {
    df <- get_fit_params(xs, error = FALSE)
    purrr::walk(l, function(id) {
      model_id <- df[df$i == id,]$grp_dat[[1]] |> as.integer()
      df_at_model_id <- df[df$grp_dat == model_id,]
      param_index <- which(df_at_model_id$i == id)
      
      m <- xs$xspec$AllModels(model_id)
      
      param <- xext_model_comp_id(m, param_index)
      param$untie()
    })
    xs$fit_params <- get_fit_params.xs(xs)
  }
  
  invisible(xs)
}

#' @export
freeze_params <- function(xs, ...) {
  UseMethod("freeze_params")
}

#' @export
freeze_params.xs <- function(xs, ...) {
  frozen_params(xs, freeze = TRUE, ...)
}

#' @export
thaw_params <- function(xs, ...) {
  UseMethod("thaw_params")
}

#' @export
thaw_params.xs <- function(xs, ...) {
  frozen_params(xs, freeze = FALSE, ...)
}


frozen_params <- function(xs, freeze = TRUE, ...) {
  l <- list(...)  |> unlist()
  if (length(l) > 0) {
    df <- get_fit_params(xs, error = FALSE)
    
    purrr::walk(l, function(id) {
      checkmate::assert_int(id, lower = 0, upper = max(df$i))
      model_id <- df[df$i == id,]$grp_dat[[1]] |> as.integer()
      df_at_model_id <- df[df$grp_dat == model_id,]
      param_index <- which(df_at_model_id$i == id)
      
      m <- xs$xspec$AllModels(model_id)
      
      param <- xext_model_comp_id(m, param_index)
      param$frozen = freeze
    })
    xs$fit_params <- get_fit_params.xs(xs)
    xs <- update_fit_stat(xs)
  }
  invisible(xs)
}


#' @export
get_eqwidth <- function(xs, comp = NULL, err = FALSE, number = 100L, level = 90, model_grp = 1L) {
  
  xspec <- xs$xspec
  xspec$AllModels$eqwidth(component = comp, err = err, number = number, level = level)
  
  assertthat::assert_that(level > 0 && level < 100)
  
  eqwidth <- xspec$AllData(model_grp)$eqwidth |> 
    reticulate::py_to_r() |> 
    unlist()
  
  tibble(
    best = eqwidth[1],
    lo = eqwidth[2],
    hi = eqwidth[3],
    level = level
  )
}


#' @export
steppar_param <- function(xs, arg, lo, up, nstep = 10L, plot = TRUE, delstat = 1) {
    xspec <- xs$xspec
    checkmate::assert_number(arg, lo = 1)
    checkmate::assert_number(nstep, lo = 2)
    checkmate::assert_logical(plot)
    arg <- as.integer(arg)
    nstep <- as.integer(nstep)

    xspec$Fit$steppar(paste(arg, lo, up, nstep))
    df <- tibble(
        val = xspec$Fit$stepparResults(arg) |> py_to_r(),
        stat = xspec$Fit$stepparResults("statistic") |> py_to_r(),
        delstat = xspec$Fit$stepparResults("delstat") |> py_to_r()
    )
    class(df) <- c("xs_steppar", class(df))

    if (plot) {
        print(plot_steppar.xs_steppar(df, delstat))
    }

    return(df)
}

#' @export
steppar_auto <- function(xs, nstep = 10, plot = TRUE) {
  UseMethod("steppar_auto")
}

#' @export
steppar_auto.xs <- function(xs, nstep = 10, plot = TRUE) {
    if ( ! all(c("lower", "upper") %in% colnames(xs$fit_params))) {
        stop("Calc errors at first")
    }
    df_tmp <- xs$fit_params |> 
        filter(frozen == "", link == "") |> 
        mutate(
          lower = ifelse(grepl("hit hard lower limit", decoding), bot, lower),
          upper = ifelse(grepl("hit hard upper limit", decoding), top, upper)
        ) |> 
        dplyr::rowwise() |> 
        mutate(steppar_df = purrr::pmap(list(i, lower, upper), \(x, y, z) steppar_param(xs, x, y, z, plot = FALSE, nstep = nstep))) |> 
        dplyr::ungroup() |> 
        select(i, comp, param, steppar_df) |> 
        tidyr::unnest(steppar_df)

    df_tmp_best <- df_tmp |> 
        dplyr::group_by(i) |> 
        filter(stat == min(stat))

    if (plot)
      print(
        df_tmp |> 
                ggplot2::ggplot(ggplot2::aes(val, stat, group = factor(i))) +
                ggplot2::geom_path() +
                ggplot2::geom_vline(data = df_tmp_best, ggplot2::aes(xintercept = val), linetype = "dashed", color = "red") +
                ggplot2::geom_hline(data = df_tmp_best, ggplot2::aes(yintercept = stat), linetype = "longdash") +
                ggplot2::geom_hline(data = df_tmp_best, ggplot2::aes(yintercept = stat + 1), linetype = "longdash") +
                ggplot2::facet_wrap(comp ~ param, scales = "free_x")
      )
      
    
    invisible(df_tmp)
}

#' @export
steppar_param2d <- function(xs, arg1, lo1, up1, nstep1, arg2, lo2, up2, nstep2) {
    xspec <- xs$xspec
    checkmate::assert_integerish(c(arg1, arg2), lower = 1)
    checkmate::assert_integerish(c(nstep1, nstep2), null.ok = FALSE, lower = 2)
    
    arg1 <- as.integer(arg1)
    arg2 <- as.integer(arg2)
    nstep1 <- as.integer(nstep1)
    nstep2 <- as.integer(nstep2)

    xspec$Fit$steppar(paste(arg1, lo1, up1, nstep1, arg2, lo2, up2, nstep2))

    df <- tibble(
        val1 = xspec$Fit$stepparResults(arg1) |> py_to_r(),
        val2 = xspec$Fit$stepparResults(arg2) |> py_to_r(),
        stat = xspec$Fit$stepparResults("statistic") |> py_to_r(),
        delstat = xspec$Fit$stepparResults("delstat") |> py_to_r()
    )
    class(df) <- c("xs_steppar2d", class(df))

    return(df)
}


#' @export
calc_error_from_steppar <- function(out_steppar, delstat) {
  UseMethod("calc_error_from_steppar")
}

#' @export
calc_error_from_steppar.xs_steppar <- function(out_steppar, delstat = 1) {
  min_stat <- min(out_steppar$stat)
  max_stat <- max(out_steppar$stat)

  checkmate::assert_number(delstat, up = max_stat - min_stat)

  conf_level <- round(pchisq(delstat, 1), 3) |> paste(... = _, "%")
  val_at_delstat <- approx(x = out_steppar$stat, y = out_steppar$val, xout = min_stat + delstat)
  out <- val_at_delstat$y
  names(out) <- conf_level
  out
}



#' @export
print.xs_fit_par <- function(x, ..., html = TRUE) {
  stopifnot(is.data.frame(x))

  df <- x
  df$grp_dat <- paste("Data group", df$grp_dat)

  if (html && is_irkernel()) {
    df |>
      dplyr::select(-grp_dat) |>
      kableExtra::kbl(booktabs = TRUE, format = "html") |>
      kableExtra::kable_styling(
        bootstrap_options = "hover",
        font_size = 14
      ) |>
      kableExtra::pack_rows(
        index = table(df$grp_dat),
        label_row_css = "background-color: lightgrey"
      ) |>
      as.character() |>
      IRdisplay::display_html()
  } else {
    class(df) <- "data.frame"
    print(df)
  }

  invisible(x)
}