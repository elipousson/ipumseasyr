#' Lookup NHGIS State Code
#'
#' @param state State name, abbreviation, FIPS code, or NHGIS code
#' @keywords internal
#' @export
lookup_nhgis_state <- function(state) {
  check_installed("tigris")

  state_name <- tolower(tigris::fips_codes[["state_name"]])
  state_abb <- tolower(tigris::fips_codes[["state"]])
  state <- tolower(state)

  if (state %in% state_name) {
    state <- tigris::fips_codes[state_name == state, "state_code"]
  } else if (state %in% state_abb) {
    state <- tigris::fips_codes[state_abb == state, "state_code"]
  }

  state <- unique(state)

  if (state %in% tigris::fips_codes[["state_code"]]) {
    state <- paste0(state, "0")
  }

  state
}

#' List NHGIS Shapefiles
#'
#' @keywords internal
#' @export
list_nhgis_shapefiles <- function(
  geography = c("county", "state"),
  year,
  extent = "us",
  basis = 2008,
  validate = FALSE,
  ...,
  api_key = Sys.getenv("IPUMS_API_KEY")
) {
  if (extent != "us") {
    extent <- lookup_nhgis_state(extent)
  }

  geography <- dplyr::case_match(
    geography,
    "state" ~ "state",
    "county" ~ "county",
    "tract" ~ "tract",
    "block_group" ~ "blck_grp",
    "block" ~ "block",
    "county_subdivision" ~ "cty_sub",
    # "" ~ "smsa",
    # "" ~ "scsa",
    # "" ~ "votedist",
    # "" ~ "place",
    # "" ~ "cd103rd",
    # "" ~ "cd101st",
    # "" ~ "aianhh",
    # "" ~ "msa_cmsa",
    # "" ~ "pmsa",
    # "" ~ "necma",
    # "" ~ "urb_area",
    # "" ~ "cd106th",
    # "" ~ "puma",
    # "" ~ "res_only",
    # "" ~ "trust",
    # "" ~ "trbl_sub",
    "zcta" ~ "zcta"
  )

  basis_values <- c(
    "tl2000",
    "tl2008",
    "tl2009",
    "tl2010",
    "tlgnis",
    "cenpop2000"
  )

  if (!(basis %in% basis_values)) {
    basis <- paste0("tl", basis)
  }

  shapefiles <- stringr::str_c(
    extent,
    geography,
    year,
    basis,
    sep = "_"
  )

  if (validate) {
    shapefiles_metadata <- ipumsr::get_metadata_nhgis(
      type = "shapefiles",
      ...,
      api_key = api_key
    )

    shapefiles <- rlang::arg_match(
      shapefiles,
      values = shapefiles_metadata[["name"]],
      multiple = TRUE
    )
  }

  shapefiles
}
