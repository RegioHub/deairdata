assert_integer <- function(x) {
  if (length(x) != 1 || !is.finite(x) || floor(x) != x) {
    stop(deparse(substitute(x)), " must be an integer", call. = FALSE)
  }
}

assert_date <- function(x) {
  x_date <- as.Date(x, format = "%Y-%m-%d")

  if (!length(x_date) || is.na(x_date)) {
    stop(deparse(substitute(x)), " must be a string <YYYY-MM-DD>", call. = FALSE)
  }
}

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
  x %>%
    rbind_list() %>%
    `colnames<-`(col_names) %>%
    as_tibble_maybe() %>%
    set_col_types(col_types)
}

rbind_list <- function(x) {
  stopifnot(is.list(x))

  tryCatch(
    do.call("rbind", x),
    warning = function(w) {
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
    identity = identity,
    logical = function(x) as.logical(as.integer(x)),
    integer = as.integer,
    double = as.double,
    character = function(x) ifelse(nzchar(x), as.character(x), NA_character_),
    factor = as.factor,
    date = function(x) as.Date(as.character(x)),
    datetime = function(x) as.POSIXct(as.character(x)),
    stop("Invalid `col_type`")
  )

  coercer(x)
}
