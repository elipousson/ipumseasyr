.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("nhgis_ts_tables", "usa_states"),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c("GISJOIN", "DATAYEAR", "STUSPS", "YEAR")
)
