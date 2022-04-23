.onLoad <- function(libname, pkgname) {
  airdata_api <<- memoise::memoise(airdata_api)

  if (!"deairdata_lang" %in% names(options())) {
    options(deairdata_lang = "en")
  }
}
