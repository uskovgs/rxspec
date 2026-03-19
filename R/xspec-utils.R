#' @export
err_status_code <- function() {
  c(
    "new minimum found",
    "non-monotonicity detected",
    "minimization may have run into problem",
    "hit hard lower limit",
    "hit hard upper limit",
    "parameter was frozen",
    "search failed in -ve direction",
    "search failed in +ve direction",
    "reduced chi-squared too high"
  )
}

#' @export
additive_components <- function() {
  c(
    "agauss", "apec", "bapec", "bbody", "bbodyrad",
    "bexrav", "bexriv", "bkn2pow", "bknpower", "bmc",
    "bremss", "btapec", "bvapec", "bvtapec", "bvvapec",
    "bvvtapec", "c6mekl", "c6pmekl", "c6pvmkl", "c6vmekl",
    "carbatm", "cemekl", "cevmkl", "compLS", "compPS",
    "compST", "compTT", "compbb", "compmag", "comptb", "compth",
    "cplinear", "cutoffpl", "zcutoffpl", "disk", "diskbb", "diskir", "diskline",
    "diskm", "disko", "diskpbb", "diskpn", "eplogpar", "eqpair",
    "eqtherm", "equil", "expdec", "ezdiskbb", "gadem", "gaussian",
    "gnei", "grad", "grbm", "hatm", "kerrbb", "kerrd", "kerrdisk",
    "laor", "laor2", "logpar", "lorentz", "meka", "mekal", "mkcflow",
    "nsmaxg", "nsx", "nteea", "nthComp", "optxagn", "optxagnf",
    "pegpwrlw", "pexmon", "pexrav", "pexriv", "plcabs", "posm",
    "powerlaw", "pshock", "raymond", "redge", "refsch", "rnei",
    "sedov", "sirf", "slimbh", "smaug", "snapec", "srcut", "sresc",
    "step", "tapec", "vapec", "vbremss", "vequil", "vgadem", "vgnei",
    "vmcflow", "vmeka", "vmekal", "vnei", "vnpshock", "voigt", "vpshock",
    "vraymond", "vrnei", "vsedov", "vtapec", "vvapec", "vvgnei", "vvnei",
    "vvnpshock", "vvpshock", "vvrnei", "vvsedov", "vvtapec", "zagauss",
    "zbbody", "zbremss", "zgauss", "zpowerlw", "relxill", "relline"
  )
}



#' @export
show_xspec_query <- function(xs) {
  o1 <- show_xspec_query_data(xs)
  o2 <- show_xspec_query_model(xs)
  c(o1, o2)
}


#' @export
show_xspec_query_data <- function(xs) {
    out1 <- xs$files |> 
        select(dirs, specs, grp_dat, grp_plt) |> 
        purrr::pmap_chr(~ paste0(
            "cd ", ..1, ";\n",
            "data ", ..3, ":", ..4, " ", ..2, ";\n"
        )) |> 
        paste(collapse = "")

    out2 <- xs$files |> 
        select(grp_dat, noticed_channels) |> 
        purrr::pmap_chr(~ paste0(
            "ignore ", ..1, ": **-**;\n",
            "notice ", ..1, ": ", ..2, ";\n"
        )) |> 
        paste(collapse = "")
      
      paste(out1, out2, collapse = "\n")
    
}


#' @export
show_xspec_query_model <- function(xs) {
    out1 <- paste("model", xs$model, "")
    out2 <- xs$fit_params |> 
        select(val, frozen, link, delta, min, bot, top, max) |> 
        purrr::pmap_chr( ~ {
            if (grepl("p", ..3)) {
                paste0("& ", gsub(" ", "", ..3))
            } else {
                paste(
                   "&", ..1, ..4, ..5, ..6, ..7, ..8
                )
            }
        }
        ) |> 
        paste(collapse = " ")
    paste(out1, out2)
}

#' @export
save_xcm <- function(xs, file = NULL, info = "a") { 
  UseMethod("save")
}

#' @export
save_xcm.xs <- function(xs, file = NULL, info = "a") {
  
  if (! checkmate::test_file_exists(file)) {
    xspec <- xs$xspec
    xspec$XspecSettings$save(xspec, fileName = file, info = info)
  } else {
    cat(glue::glue("File {file} already exists"))
  }
  invisible(xs)
}

#' @export
get_all_info <- function(xs,
                         error = FALSE,
                         plot = "data",
                         x = "keV") {
  UseMethod("get_all_info")
}

#' @export
get_all_info.xs <-
  function(xs,
           error = FALSE,
           plot = "data",
           x = "keV") {

    tibble(
      model = xs$model,
      spec = list(xs$files),
      fit = list(get_fit_params.xs(xs, error = error)),
      fit_settings = list(xs$fit_settings)
    )  |> 
      mutate(stat = list(xs$stat),
             plot = list(get_plot_data.xs(xs, plot = plot, x = x)))

  }