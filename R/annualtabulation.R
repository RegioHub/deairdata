#' @export
airdata_extract.annualtabulation <- function(resp) {
  parsed <- httr2::resp_body_json(resp)

  parsed$data |>
    purrr::map_dfr(~ purrr::set_names(as.integer(.x[1:2]), c("station", "value")))
}
