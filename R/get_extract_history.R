#' Get NHGIS Extract history
#'
#' @inheritDotParams ipumsr::get_extract_history
#' @keywords internal
#' @export
get_nhgis_extract_history <- function(...) {
  check_installed("withr")

  withr::with_envvar(
    c("IPUMS_DEFAULT_COLLECTION" = "nhgis"),
    {
      ipumsr::get_extract_history(...)
    }
  )
}
