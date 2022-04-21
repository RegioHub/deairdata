resp_body_json2 <- function(resp) {
  httr2::resp_body_json(
    resp,
    simplifyVector = TRUE, simplifyMatrix = FALSE, simplifyDataFrame = FALSE
  )
}

keep_numbered <- function(x) {
  if (is.null(names(x))) return(x)

  x[grep("^\\d+$", names(x))]
}

as_tibble2 <- function(x, col_names, col_types) {
  x |>
    rbind_list() |>
    `colnames<-`(col_names) |>
    as_tibble_maybe() |>
    set_col_types(col_types)
}

rbind_list <- function(x) {
  stopifnot(is.list(x))

  tryCatch(
    do.call("rbind", x),
    warning = \(w) {
      msg <- conditionMessage(w)
      if (startsWith(msg, "number of columns")) stop(msg)
    }
  )
}

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
    logical = \(x) as.logical(as.integer(x)),
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
