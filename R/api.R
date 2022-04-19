airdata_api <- function(endpoint, ...) {
  stopifnot(endpoint %in% endpoints$name)

  path <- endpoints$path[endpoints$name == endpoint]

  resp <- httr2::request(api_spec$servers[[1]]$url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(...) |>
    httr2::req_user_agent("https://github.com/long39ng/deairdata") |>
    httr2::req_error(body = api_error_body) |>
    httr2::req_retry(3) |>
    httr2::req_perform()

  resp$path <- path

  class(resp) <- c(class(resp), endpoint)

  resp
}

api_error_body <- function(resp) {
  if (httr2::resp_status(resp) == 422) {
    "Required parameter(s) missing."
  } else {
    NULL
  }
}

airdata_extract <- function(x) {
  UseMethod("airdata_extract")
}

airdata_extract_parsed <- function(x) {
  UseMethod("airdata_extract_parsed")
}
