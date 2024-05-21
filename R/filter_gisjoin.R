#' Filter a NHGIS data frame by a state FIPS and county FIPS numbers
#'
#' By default, filter to counties in the Baltimore metro area.
#'
#' @param values Optional vector of GISJOIN values. state_fips and county_fips
#'   are ignored if this is supplied.
#' @keywords internal
filter_gisjoin <- function(data,
                           state = NULL,
                           county = NULL,
                           values = NULL,
                           gisjoin_col = "GISJOIN") {
  if (is.null(values)) {
    check_installed("tigris")

    if (!is.null(state) && is.null(county)) {
      state <- tigris:::validate_state(state = state, multiple = TRUE)
      values <- paste0("G", state, "0")
    } else {
      state <- tigris:::validate_state(state = state)
      county <- tigris:::validate_state(state = state, county = county, multiple = TRUE)
      values <- paste0("G", state, "0", county, "0")
    }
  }

  dplyr::filter(data, .data[[gisjoin_col]] %in% values)
}
