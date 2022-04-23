#' @export
airdata_extract_parsed.annualbalances <- function(parsed) {
  parsed %>%
    lapply(function(x) x[1:2]) %>%
    as_tibble2(col_names = c("station", "value"), col_types = rep("integer", 2))
}
