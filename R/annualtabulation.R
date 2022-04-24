#' Get annual tabulations
#'
#' Returns the annual balances for a component by station
#'
#' @inheritParams air_exceedances
#'
#' @export
#'
#' @examples
#' \dontrun{
#' air_annual(5, 2020)
#' }
air_annual <- function(component, year) {
  assert_integer(year)
  lang <- getOption("deairdata_lang", "en")
  airdata_call("annualbalances")
}

#' @export
airdata_extract_parsed.annualbalances <- function(parsed) {
  parsed %>%
    lapply(function(x) x[1:2]) %>%
    as_tibble2(col_names = c("station", "value"), col_types = c("integer", "double"))
}
