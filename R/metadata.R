#' @export
airdata_extract.meta <- function(resp) {
  parsed <- resp_body_json2(resp)

  use <- parsed$request$use

  parsed <- mapply(
    \(x, y) x |>
      `class<-`(c(class(x), y)) |>
      `attr<-`("use", use),
    parsed, names(parsed),
    SIMPLIFY = FALSE
  )

  parsed |>
    Filter(f = \(x) !(inherits(x, "request") | inherits(x, "years"))) |>
    lapply(airdata_extract_parsed)
}

#' @export
airdata_extract_parsed.components <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("component", "code", "symbol", "unit", "name"),
    col_types = c("integer", rep("character", 4))
  )
}

#' @export
airdata_extract_parsed.scopes <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("scope", "code", "time_base", "time_scope_secs", "time_is_max", "name"),
    col_types = c("integer", rep("character", 2), "integer", "logical", "character")
  )
}

#' @export
airdata_extract_parsed.networks <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("network", "code", "name"),
    col_types = c("integer", rep("character", 2))
  )
}

#' @export
airdata_extract_parsed.stationsettings <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("station_setting", "name", "name_short"),
    col_types = c("integer", rep("character", 2))
  )
}

#' @export
airdata_extract_parsed.stationtypes <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("station_type", "name"),
    col_types = c("integer", "character")
  )
}

#' @export
airdata_extract_parsed.stations <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c(
      "station",
      "station_code",
      "station_name",
      "city",
      "station_synonym",
      "first_active",
      "last_active",
      "long",
      "lat",
      "network",
      "setting",
      "type",
      "network_code",
      "network_name",
      "setting_name",
      "setting_name_short",
      "type_name",
      "street",
      "street_number",
      "zip_code"
    ),
    col_types = c(
      "integer",
      rep("character", 4),
      rep("date", 2),
      rep("double", 2),
      rep("integer", 3),
      rep("character", 8)
    )
  )
}

#' @export
airdata_extract_parsed.thresholds <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("threshold", "component", "scope", "type", "min", "max", "index"),
    col_types = c(rep("integer", 3), "character", rep("integer", 3))
  )
}

#' @export
airdata_extract_parsed.transgressiontypes <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("exceedance_type", "name"),
    col_types = c("integer", "character")
  )
}

#' @export
airdata_extract_parsed.limits <- function(parsed) {
  if (isTRUE(attr(parsed, "use") == "annualbalance")) {
    return(
      as_tibble2(
        parsed,
        col_names = c("year", "component"),
        col_types = rep("integer", 2)
      )
    )
  }

  general_limits <- Filter(\(x) length(x) == 2, parsed)

  station_limits <- Filter(\(x) length(x) == 5, parsed)

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

  as_tibble2(
    c(station_limits, general_limits),
    col_names = c("scope", "component", "station", "min_start_dttm", "max_start_dttm"),
    col_types = c(rep("integer", 3), rep("datetime", 2))
  )
}

#' @export
airdata_extract_parsed.xref <- function(parsed) {
  as_tibble2(
    parsed,
    col_names = c("component", "scope", "has_map", "has_alt_map", "is_hourly"),
    col_types = c(rep("integer", 2), rep("logical", 3))
  )
}
