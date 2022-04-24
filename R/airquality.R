#' Get air quality data
#'
#' `air_quality()` returns air quality data for the provided parameters.
#'
#' @inheritParams air_stations
#' @param station ID of the [station][air_stations] to get data from. If `NULL`,
#'   include all available stations.
#'
#' @return A named list where the names are station IDs and each element is a
#'   [nested data frame](https://tidyr.tidyverse.org/articles/nest.html) for the
#'   corresponding station with 5 columns:
#'   * start_dttm, end_dttm: Time of start and end of measuring in CET
#'   * index: Airquality index for all components
#'   * is_incomplete: Flag if data is incomplete (not all components available)
#'   * data: List column of data frames with the measured components
#'     * component: Component ID
#'     * value: Measured value for this component
#'     * index: Airquality index of this component
#'     * pct_in_index_thresholds: Decimal representation of percent in index
#'       thresholds. 0.x is x percent between index 0 and 1, 1.x is x percent
#'       between index 1 and 2 etc.
#' @export
#'
#' @examples
#' \dontrun{
#' air_quality("2020-01-01", 9, "2020-01-01", 11, station = 7)
#' }
air_quality <- function(date_from, time_from, date_to, time_to, station = NULL) {
  assert_date(date_from)
  assert_date(date_to)
  assert_integer(time_from)
  assert_integer(time_to)
  if (!is.null(station)) assert_integer(station)

  airdata_call("airquality")
}

#' Get air quality date limits
#'
#' `air_quality_date_limits()` returns the date limits of air quality data by
#'   [station][air_stations].
#'
#' @rdname air_quality
#'
#' @export
air_quality_date_limits <- function() {
  airdata_call("airqualitylimits")
}

#' @export
airdata_extract_parsed.airquality <- function(parsed) {
  lapply(
    parsed,
    function(x) as_tibble2(
      mapply(
        function(x, y) airquality_by_station_hour(x, start_dttm = y),
        x, names(x),
        SIMPLIFY = FALSE
      ),
      col_names = c("start_dttm", "end_dttm", "index", "is_incomplete", "data"),
      col_types = c(rep("datetime", 2), "integer", "logical", "identity")
    )
  )
}

airquality_by_station_hour <- function(x, start_dttm) {
  stopifnot(is.list(x) && length(x) > 3)

  data <- as_tibble2(
    x[4:length(x)],
    col_names = c("component", "value", "index", "pct_in_index_thresholds"),
    col_types = c("integer", "double", "integer", "double")
  )

  c(list(start_dttm), x[1:3], list(data))
}

#' @export
airdata_extract_parsed.airqualitylimits <- function(parsed) {
  as_tibble2(
    mapply(
      function(x, y) c(y, x),
      parsed, names(parsed),
      SIMPLIFY = FALSE
    ),
    col_names = c("station", "min_start_dttm", "max_start_dttm"),
    col_types = c("integer", rep("datetime", 2))
  )
}
