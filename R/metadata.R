#' @export
airdata_extract.meta <- function(resp) {
  parsed <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  parsed <- mapply(
    \(x, y) `class<-`(x, c(class(x), y)),
    parsed, names(parsed),
    SIMPLIFY = FALSE
  )

  parsed |>
    Filter(f = \(x) is(x, "limits")) |>
    lapply(airdata_extract_parsed)
}

#' @export
airdata_extract_parsed.limits <- function(parsed) {
  general_limits <- parsed |>
    Filter(f = \(x) length(x) == 2)

  station_limits <- parsed |>
    Filter(f = \(x) length(x) == 5)

  stopifnot(length(parsed) == length(general_limits) + length(station_limits))

  general_limits <- mapply(
    \(x, y) c(
      unlist(strsplit(y, "", fixed = TRUE)), # scope, component
      NA_character_, # station
      x # min, max start date
    ),
    general_limits, names(general_limits),
    SIMPLIFY = FALSE
  )

  station_limits |>
    c(general_limits) |>
    do.call(what = rbind) |>
    `colnames<-`(c("scope", "component", "station", "min_start_dttm", "max_start_dttm")) |>
    as_tibble_maybe() |>
    set_col_types(c(rep("integer", 3), rep("datetime", 2)))
}
