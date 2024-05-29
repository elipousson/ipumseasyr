#' Get IPUMS Extract histories
#'
#' @inheritDotParams ipumsr::get_extract_history -collection
#' @keywords internal
#' @name ipums_extract_history
NULL

#' @rdname ipums_extract_history
#' @name get_nhgis_extract_history
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

#' @rdname ipums_extract_history
#' @name get_usa_extract_history
#' @export
get_usa_extract_history <- function(...) {
  check_installed("withr")

  withr::with_envvar(
    c("IPUMS_DEFAULT_COLLECTION" = "usa"),
    {
      ipumsr::get_extract_history(...)
    }
  )
}

#' @rdname ipums_extract_history
#' @name get_cps_extract_history
#' @export
get_cps_extract_history <- function(...) {
  check_installed("withr")

  withr::with_envvar(
    c("IPUMS_DEFAULT_COLLECTION" = "cps"),
    {
      ipumsr::get_extract_history(...)
    }
  )
}

#' @rdname ipums_extract_history
#' @name get_ipumsi_extract_history
#' @export
get_ipumsi_extract_history <- function(...) {
  check_installed("withr")

  withr::with_envvar(
    c("IPUMS_DEFAULT_COLLECTION" = "ipumsi"),
    {
      ipumsr::get_extract_history(...)
    }
  )
}
