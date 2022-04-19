.onLoad <- function(libname, pkgname) {
  airdata_api <<- memoise::memoise(airdata_api)
}
