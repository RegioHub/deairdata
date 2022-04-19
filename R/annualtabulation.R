#' @export
airdata_extract.annualbalances <- function(resp) {
  parsed <- httr2::resp_body_json(resp)

  parsed$data |>
    lapply(\(x) setNames(as.integer(x[1:2]), c("station", "value"))) |>
    do.call(what = rbind) |>
    as_tibble_maybe()
}
