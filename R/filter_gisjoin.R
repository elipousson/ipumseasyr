#' Filter a NHGIS data frame by a state FIPS and county FIPS numbers
#'
#' By default, filter to counties in the Baltimore metro area.
#'
#' @param values Optional vector of GISJOIN values. state_fips and county_fips
#'   are ignored if this is supplied.
#' @keywords internal
filter_nhgisjoin <- function(data,
                             state = NULL,
                             county = NULL,
                             values = NULL,
                             gisjoin_col = "NHGISJOIN") {
  if (is.null(values)) {
    check_installed("tigris")

    if (!is.null(state) && is.null(county)) {
      state <- tigris::validate_state(state = state, multiple = TRUE)
      pattern <- paste0(paste0("^G", state, "0"), collapse = "|")
    } else {
      state <- tigris::validate_state(state = state)
      county <- tigris::validate_county(state = state, county = county, multiple = TRUE)
      pattern <- paste0(paste0("^G", state, "0", county, "0"), collapse = "|")
    }

    return(dplyr::filter(data, stringr::str_detect(.data[[gisjoin_col]], pattern)))
  }

  dplyr::filter(data, .data[[gisjoin_col]] %in% values)
}
