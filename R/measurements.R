#' Get all measurements
#'
#' `air_measures()` returns measures for the provided parameters.
#'
#' @inheritParams air_quality
#' @param component ID of [component][air_components]. If NULL, include all available components.
#' @param scope ID of [scope][air_scopes]. If NULL, include all available scopes.
#'
#' @return A named list where the names are station IDs and each element is a
#'   data frame for the corresponding station
#' @export
#'
#' @examples
#' \dontrun{
#' air_measures("2020-01-01", 9, "2020-01-01", 11)
#' }
air_measures <- function(date_from, time_from, date_to, time_to,
                         station = NULL, component = NULL, scope = NULL) {
  assert_date(date_from)
  assert_date(date_to)
  assert_integer(time_from)
  assert_integer(time_to)
  if (!is.null(station)) assert_integer(station)
  if (!is.null(component)) assert_integer(component)
  if (!is.null(scope)) assert_integer(scope)

  airdata_call("measures")
}

#' Get measurement date limits
#'
#' `air_measures_date_limits()` returns the date limits of measurements by
#'   [scope][air_scopes], [component][air_components], and [station][air_stations].
#'
#' @rdname air_measures
#'
#' @export
air_measures_date_limits <- function() {
  airdata_call("measureslimits")
}

#' @export
airdata_extract_parsed.measures <- function(parsed) {
  parsed %>%
    lapply(
      function(x) as_tibble2(
        mapply(
          FUN = function(x, y) c(x[1:3], y, x[4:5]),
          x, names(x),
          SIMPLIFY = FALSE
        ),
        col_names = c("component", "scope", "value", "start_dttm", "end_dttm", "index"),
        col_types = c(rep("integer", 2), "double", rep("datetime", 2), "integer")
      )
    )
}

#' @export
airdata_extract.measureslimits <- function(resp) {
  parsed <- resp_body_json2(resp)

  parsed$data %>%
    `class<-`(c(class(parsed$data), "limits")) %>%
    airdata_extract_parsed()
}
