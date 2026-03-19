#' @export
clear_chain <- function(xs) {
  UseMethod("clear_chain")
}

#' @export
clear_chain.xs <- function(xs) {
  xspec <- xs$xspec
  xspec$AllChains$clear()
  invisible(xs)
}

#' @export
load_chain <- function(xs, file) {
  UseMethod("load_chain")
}

#' @export
load_chain.xs <- function(xs, file) {
  checkmate::assert_file_exists(file)
  xspec <- xs$xspec

  
  clear_chain.xs(xs)
  xext_py_add_operator(xspec$AllChains, file)
  cat("loading chain ", file, "\n")
  xs$chain <- reticulate::py_to_r(xspec$AllChains(1L)$fileName)
  
  invisible(xs)
}

#' @export
run_chain <- function(xs, file, overwrite = FALSE) {
  UseMethod("run_chain")
}

#' @export
run_chain.xs <- function(xs, file, overwrite = FALSE) {
  
  xspec <- xs$xspec
  xs$chain_settings |> 
    knitr::kable(format = "html") |> 
    as.character() |> 
    IRdisplay::display_html()
  
  if(checkmate::test_file_exists(file) && overwrite) {
    file.remove(file)
    print(glue::glue("File {file} was overwritten\n"))
  } 
  
  if(!checkmate::test_file_exists(file)) {
    clear_chain(xs)
    chain = xspec$Chain(file)
    chain$run()
    xs$chain <- reticulate::py_to_r(chain$fileName)
  } else {
    print(glue::glue("File {file} already exists. Use option `overwrite=TRUE`"))
  }
  
  invisible(xs)
}



read_chain_as_df <- function(file) {
  if(checkmate::test_file_exists(file)) {
    Rfits::Rfits_read_table(file) |> 
      janitor::clean_names()
  } else {
    checkmate::check_file_exists(file)
    return(NULL)
  }
}

#' @export
set_chain_settings <- function(xs, 
                               alg = "gw", 
                               burn = 100L, 
                               length = 1000L, 
                               proposal = "gaussian fit",
                               rand = FALSE,
                               rescale = NULL,
                               temp = 1.0,
                               walkers = 10L) {
  checkmate::assert_choice(alg, choices = c("gw", "mh"))
  checkmate::assert_int(burn, lower = 1)
  checkmate::assert_int(length, lower = 1)
  checkmate::assert_choice(proposal, choices = c("gaussian fit", "gaussian deltas 100"))
  checkmate::assert_logical(rand, len = 1)
  checkmate::assert_number(rescale, null.ok = TRUE)
  checkmate::assert_number(temp, lower = 1e-9)
  checkmate::assert_int(walkers, lower = 1)
  
  xspec <- xs$xspec
  xspec$AllChains$defAlgorithm <- alg
  xspec$AllChains$defBurn <- as.integer(burn)
  xspec$AllChains$defFileType <- "fits"
  xspec$AllChains$defProposal <- proposal
  xspec$AllChains$defRand <- rand
  if (is.null(rescale)) {
    xspec$AllChains$defRescale <- reticulate::py_none()
  } else {
    xspec$AllChains$defRescal <- rescale
  }
  xspec$AllChains$defTemperature <- temp
  xspec$AllChains$defWalkers <- as.integer(walkers)
  
  xspec$AllChains$defLength <- as.integer(length)
  if (length %% walkers != 0) {
    cat("Length must be a multiple of the number of walkers.\nModifying length to", 
      reticulate::py_to_r(xspec$AllChains$defLength), "\n")
  }
    
  update_chain_settings(xs)
  invisible(xs)
}

update_chain_settings <- function(xs) {
  UseMethod("update_chain_settings")
}
update_chain_settings.xs <- function(xs) {
  xspec <- xs$xspec
  xs$chain_settings <- tibble(
    file_format = reticulate::py_to_r(xspec$AllChains$defFileType),
    chain_type = reticulate::py_to_r(xspec$AllChains$defAlgorithm),
    length = reticulate::py_to_r(xspec$AllChains$defLength),
    burn = reticulate::py_to_r(xspec$AllChains$defBurn),
    walkers = reticulate::py_to_r(xspec$AllChains$defWalkers)
  )
  
  invisible(xs)
}