#' @export
load_data <- function(x,
                      specs,
                      grp_dat = NULL,
                      grp_plt = NULL) {
  UseMethod("load_data")
}

#' @export
load_data.xs <-
  function(x,
           specs,
           grp_dat = NULL,
           grp_plt = NULL) {
    
    xspec <- x$xspec

    if (specs[1] == "none" && length(specs) == 1L) {
      xspec$AllData(paste0(grp_dat, ":", grp_plt, " ", specs))
      x$files <- x$files[- grp_dat, ]
      cat(grp_dat, ":", grp_plt, " was removed", sep = " ")
      cat("\n")
    } else {
      checkmate::assert_file_exists(specs)
      original_path <- getwd()
      spec_dirs <- dirname(specs)
      spec_names <- basename(specs)
      n <- length(specs)
      n_loaded <- reticulate::py_to_r(xspec$AllData$nSpectra)
  


      is_null <- c(is.null(grp_dat), is.null(grp_plt))
      if (all(is_null)) {
        grp_dat <- rep(1, n)
        grp_plt <- rep(1, n)
      }
      if (length(grp_dat) == length(grp_plt) && length(grp_dat) == n) {
        for (i in 1:n) {
          setwd(spec_dirs[i])
          data_str <-
            paste0(grp_dat[i], ":", grp_plt[i], " ", spec_names[i])
          xspec$AllData(data_str)
          cat("loading ", data_str, "\n")
        }
        new_specs <- tibble(
          specs = spec_names,
          grp_dat = grp_dat,
          grp_plt = grp_plt,
          dirs = spec_dirs
        )
        x$files <- dplyr::bind_rows(x$files, new_specs)


        setwd(original_path)
      }
    }
    update_data_info(x)
    x
  }

#' @export
ignore <- function(x, range="") {
  UseMethod("ignore")
}

#' @export
ignore.xs <- function(x, range="") {
  checkmate::assert_character(range, len = 1L)
  xspec <- x$xspec
  xspec$AllData$ignore(range)
  update_data_info(x)
  invisible(x)
}

#' @export
ignore_for_all <- function(x, range = "") {
  UseMethod("ignore_for_all")
}

#' @export
ignore_for_all.xs <- function(x, range = "") {
  checkmate::assert_character(range, len = 1L)
  xspec <- x$xspec
  n <- reticulate::py_to_r(xspec$AllData$nSpectra)
  if (n > 1) {
    xspec$AllData$ignore(glue::glue("1-{n}: {range}"))
  } else {
    xspec$AllData$ignore(range)
  }
  
  update_data_info(x)
  invisible(x)
}


#' @export
notice <- function(x) {
  UseMethod("notice")
}

#' @export
notice.xs <- function(x, range="") {
  checkmate::assert_character(range, len = 1L)
  xspec <- x$xspec
  xspec$AllData$notice(range)
  update_data_info(x)
  invisible(x)
}



update_data_info <- function(x) {
  xspec <- x$xspec
  n <- reticulate::py_to_r(xspec$AllData$nSpectra)
  exposure1 <- vector("numeric", n)
  rate_net1 <- vector("numeric", n)
  rate_net_variance1 <- vector("numeric", n)
  rate_total1 <- vector("numeric", n)
  rate_model1 <- vector("numeric", n)
  statistic1 <- vector("numeric", n)
  noticed_channels1 <- vector("character", n)
  noticed_energies1 <- vector("character", n)
  
  if (n > 0) {
    for (i in 1:n) {
      exposure1[i] <- reticulate::py_to_r(xspec$AllData(i)$exposure)
      rate <- reticulate::py_to_r(xspec$AllData(i)$rate)
      rate_net1[i] <- rate[[1]]
      rate_net_variance1[i] <- rate[[2]]
      rate_total1[i] <- rate[[3]]
      rate_model1[i] <- rate[[4]]
      noticed_channels1[i] <- reticulate::py_to_r(xspec$AllData(i)$noticedString())
      noticed_energies1[i] <- xspec$AllData(i)$energies |> 
        py_to_r() %>%
        tibble(val = .) |> 
        mutate(
          min_e = purrr::map_dbl(val, ~ .x[[1]]),
          max_e = purrr::map_dbl(val, ~ .x[[2]])
        ) |> 
        select(-val)  %>%
        mutate(grp = cumsum(max_e != dplyr::lead(
          min_e,
          default = .$max_e[n()]
        ))) |> 
        dplyr::group_by(grp)  |> 
        dplyr::summarise(
          min_e = round(min(min_e), 3),
          max_e = round(max(max_e), 3)
        ) %>%
        purrr::pmap_chr(~ paste0(..2, "-", ..3)) |> 
        paste(collapse = " ")
    
  }

    x$files <- x$files |> 
      mutate(
        exp = exposure1,
        counts_net = exp * rate_net1,
        rate_net = rate_net1,
        rate_net_variance = rate_net_variance1,
        rate_total = rate_total1,
        rate_model = rate_model1,
        noticed_energies = noticed_energies1,
        noticed_channels = noticed_channels1
      )  |> 
      select(grp_dat, grp_plt, specs, everything())  |> 
      dplyr::relocate(dirs, .after=dplyr::last_col())
  
    x
  } else {
    x
  }
}