airdata_extract_parsed.transgressions <- function(parsed) {
  parsed %>%
    lapply(function(x) {
      len <- length(x)
      if (len < 16) {
        x[(len + 1):16] <- NA_character_
      } else {
        x
      }
    }) %>%
    as_tibble2(
      col_names = c("station", "first_activity_date", "recent_activity_date", "sum", tolower(month.abb)),
      col_types = c("integer", rep("date", 2), rep("integer", 13))
    )
}
