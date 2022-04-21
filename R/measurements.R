#' @export
airdata_extract_parsed.measures <- function(parsed) {
  parsed |>
    lapply(
      \(x) as_tibble2(
        mapply(
          FUN = \(x, y) c(x[1:3], y, x[4:5]),
          x, names(x),
          SIMPLIFY = FALSE
        ),
        col_names = c("component", "scope", "value", "start_date", "end_date", "index"),
        col_types = c(rep("integer", 2), "double", rep("date", 2), "integer")
      )
    )
}

#' @export
airdata_extract.measureslimits <- function(resp) {
  parsed <- resp_body_json2(resp)

  parsed$data |>
    `class<-`(c(class(parsed$data), "limits")) |>
    airdata_extract_parsed()
}
