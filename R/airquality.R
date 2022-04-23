#' @export
airdata_extract_parsed.airquality <- function(parsed) {
  lapply(
    parsed,
    function(x) as_tibble2(
      mapply(
        function(x, y) airquality_by_station_date(x, start_dttm = y),
        x, names(x),
        SIMPLIFY = FALSE
      ),
      col_names = c("start_dttm", "end_dttm", "index", "is_incomplete", "data"),
      col_types = c(rep("datetime", 2), "integer", "logical", "identity")
    )
  )
}

airquality_by_station_date <- function(x, start_dttm) {
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
    parsed,
    col_names = c("min_start_dttm", "max_start_dttm"),
    col_types = rep("datetime", 2)
  )
}
