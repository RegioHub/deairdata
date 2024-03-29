#' Get exceedances data
#'
#' `air_exceedances()` returns exceedances for a component by station and month.
#'
#' @param year A 4-digit year
#' @inheritParams air_thresholds
#'
#' @export
#'
#' @examples
#' \dontrun{
#' air_exceedances(5, 2020)
#' }
air_exceedances <- function(component, year) {
  assert_integer(year)
  lang <- getOption("deairdata_lang", "en")
  airdata_call("transgressions")
}

#' @export
airdata_extract_parsed.transgressions <- function(parsed) {
  parsed %>%
    lapply(function(x) {
      len <- length(x)
      if (len < 16) {
        x[(len + 1):16] <- NA_character_
      }
      x
    }) %>%
    as_tibble2(
      col_names = c("station", "first_activity_date", "recent_activity_date", "sum", tolower(month.abb)),
      col_types = c("integer", rep("date", 2), rep("integer", 13))
    )
}
