library(rvest)
library(dplyr)

spec_url <- read_html("https://www.umweltbundesamt.de/daten/luft/luftdaten/doc") %>%
  html_element("redoc") %>%
  html_attr("spec-url") %>%
  paste0("https://www.umweltbundesamt.de", .)

api_spec <- yaml::read_yaml(spec_url)

endpoints <- tibble(path = names(api_spec$paths)) %>%
  mutate(name = stringr::str_remove_all(path, "(/|json)"), .before = path)

usethis::use_data(api_spec, endpoints, internal = TRUE, overwrite = TRUE)
