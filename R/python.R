get_xspec <- function() {
  x <- .rxspec$xspec

  if (is.null(x)) {
    stop("PyXspec module is not initialized.", call. = FALSE)
  }

  tryCatch(
    {
      # force delayed module load
      invisible(x$Xset$version)
      x
    },
    error = function(e) {
      stop(
        paste(
          "Cannot load Python module 'xspec'.",
          "Make sure that:",
          "1) HEASoft/PyXspec is installed,",
          "2) HEADAS is initialized before starting R,",
          "3) reticulate uses the same Python that was used to build HEASoft.",
          "",
          "Original error:",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

get_xext <- function(required = FALSE) {
  mod <- .rxspec$x_ext

  if (is.null(mod)) {
    if (required) {
      stop(
        "Python module 'xspec_extension' is not available. Put xspec_extension.py into inst/python/.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  tryCatch(
    {
      # force delayed module load
      invisible(reticulate::py_list_attributes(mod))
      mod
    },
    error = function(e) {
      if (required) {
        stop(
          paste(
            "Cannot load Python module 'xspec_extension'.",
            "Original error:",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
      NULL
    }
  )
}

has_xext <- function() {
  !is.null(get_xext(required = FALSE))
}

xext_model_comp_id <- function(model, param_index) {
  get_xext(required = TRUE)$model_comp_id(model, as.integer(param_index))
}

xext_py_add_operator <- function(obj, value) {
  get_xext(required = TRUE)$py_add_operator(obj, value)
}