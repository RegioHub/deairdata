#' Get metadata
#'
#' `air_components()` returns a list of all air components measured.
#' `air_networks()` returns a list of all air measuring station networks.
#' `air_scopes()` returns a list of all measurement scopes.
#' `air_thresholds()` returns a list of all thresholds.
#' `air_data_availability()` returns a list of available dates/years.
#'
#' @rdname air_meta
#' @export
air_components <- function() {
  lang <- getOption("deairdata_lang", "en")
  airdata_call("components")
}

#' @rdname air_meta
#' @export
air_networks <- function() {
  lang <- getOption("deairdata_lang", "en")
  airdata_call("networks")
}

#' @rdname air_meta
#' @export
air_scopes <- function() {
  lang <- getOption("deairdata_lang", "en")
  airdata_call("scopes")
}

#' Get stations
#'
#' `air_stations()` returns a list of all air measuring stations for a certain use.
#'
#' @param use Should data for "annualbalance", "airquality", "measure", or
#'   "transgression" be returned?
#' @param date_from,date_to A date ("YYYY-MM-DD", required only for use = "airquality")
#' @param time_from,time_to An hour (1..24)
#'
#' @export
air_stations <- function(use = c("annualbalance", "airquality", "measure", "transgression"),
                         date_from = NULL,
                         time_from = NULL,
                         date_to = NULL,
                         time_to = NULL,
                         return_sf = FALSE) {
  use <- match.arg(use)
  lang <- getOption("deairdata_lang", "en")

  if (use == "airquality") {
    assert_date(date_from)
    assert_date(date_to)
  }
  if (!is.null(time_from) || !is.null(time_to)) {
    assert_integer(time_from)
    assert_integer(time_to)
  }

  ret <- airdata_call("meta")[["stations"]]

  if (!return_sf) {
    return(ret)
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package {sf} needed to return the data as an sf object.")
  } else {
    sf::st_as_sf(ret, coords = c("long", "lat"), crs = sf::st_crs("EPSG:4326"))
  }
}

#' Get all station settings
#'
#' `air_station_settings()` returns a list of all station settings.
#'
#' @rdname air_stations
#' @export
air_station_settings <- function() {
  lang <- getOption("deairdata_lang", "en")
  airdata_call("stationsettings")
}

#' Get all station types
#'
#' `air_station_types()` returns a list of all station types.
#'
#' @rdname air_stations
#' @export
air_station_types <- function() {
  lang <- getOption("deairdata_lang", "en")
  airdata_call("stationtypes")
}

#' @param use Should thresholds for "airquality" or "measure" be returned?
#' @param component ID of component (see [air_components])
#' @param scope ID of scope (see [air_scopes])
#'
#' @rdname air_meta
#' @export
air_thresholds <- function(use, component = NULL, scope = NULL) {
  use <- match.arg(use, c("airquality", "measure"))
  lang <- getOption("deairdata_lang", "en")
  airdata_call("thresholds")
}

#' Get all exceedances types
#'
#' `air_exceedances_types()` returns a list of all exceedances types.
#'
#' @rdname air_exceedances
#' @export
air_exceedances_types <- function() {
  lang <- getOption("deairdata_lang", "en")
  airdata_call("transgressiontypes")
}

#' @param use Should data for "annualbalance", "airquality", or "measure" be returned?
#' @inheritParams air_stations
#'
#' @rdname air_meta
#' @export
air_data_availability <- function(use = c("annualbalance", "airquality", "measure"),
                                  date_from = NULL,
                                  time_from = NULL,
                                  date_to = NULL,
                                  time_to = NULL) {
  use <- match.arg(use)
  lang <- getOption("deairdata_lang", "en")

  if (use == "airquality") {
    assert_date(date_from)
    assert_date(date_to)
  }
  if (!is.null(time_from) || !is.null(time_to)) {
    assert_integer(time_from)
    assert_integer(time_to)
  }

  airdata_call("meta")[["limits"]]
}

#' @export
airdata_extract.meta <- function(resp) {
  parsed <- resp_body_json2(resp)

  use <- parsed$request$use

  parsed <- mapply(
    function(x, y) x %>%
      `class<-`(c(class(x), y)) %>%
      `attr<-`("use", use),
    parsed, names(parsed),
    SIMPLIFY = FALSE
  )

  parsed %>%
    Filter(f = function(x) {
      !(inherits(x, "indices") | inherits(x, "request") | inherits(x, "years"))
    }) %>%
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
      col_names = c("threshold", "component", "scope", "use", "min", "max", "index"),
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

  general_limits <- Filter(function(x) length(x) == 2, parsed)

  station_limits <- Filter(function(x) length(x) %in% c(3, 5), parsed)

  stopifnot(length(parsed) == length(general_limits) + length(station_limits))

  general_limits <- mapply(
    function(x, y) {
      scope <- substr(y, 1, 1)
      component <- substr(y, 2, 3)
      c(scope, component, NA_character_, x)
    },
    general_limits, names(general_limits),
    SIMPLIFY = FALSE
  )

  station_limits <- lapply(
    station_limits,
    function(x) {
      if (length(x) == 3) {
        x <- c(rep(NA_character_, 2), x)
      }
      x
    }
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
