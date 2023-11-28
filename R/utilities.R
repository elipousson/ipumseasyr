.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("nhgis_ts_tables"),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c("GISJOIN")
)
