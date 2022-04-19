as_tibble_maybe <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(x)
  } else {
    as.data.frame(x)
  }
}

set_col_types <- function(data, col_types) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(col_types) && length(col_types) == ncol(data))

  for (i in seq_along(col_types)) {
    data[[i]] <- set_col_type_(data[[i]], col_types[[i]])
  }

  data
}

set_col_type_ <- function(x, col_type) {
  coercer <- switch(col_type,
    logical = as.logical,
    integer = as.integer,
    double = as.double,
    character = as.character,
    factor = as.factor,
    date = as.Date,
    datetime = as.POSIXct,
    stop("Invalid `col_type`")
  )

  coercer(x)
}
