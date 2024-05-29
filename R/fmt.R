#' Join additional attribute columns: Census division, region, and USPS abbreviation to Census state column
#' @noRd
join_us_census_state <- function(data) {
  stopifnot(
    all(has_name(data, "STATE"))
  )

  data |>
    dplyr::left_join(usa_states, by = "STATE")
}


#' Format NHGIS places data by joining additional attributes and adding a label
#' column
#'
#' [fmt_nhgis_places()] joins Census division, region, and USPS abbreviation
#' columns based on the state value and creates a new label column following the
#' pattern of "<NAME column with remove pattern applied>, <USPS State
#' Abbreviation>".
#'
#' @param name_col Name column to use as the basis for the label.
#' @param label_col Label column name.
#' @param remove_pattern Passed to `pattern` for [stringr::str_remove()]
#' @keywords internal fmt
#' @export
fmt_nhgis_places <- function(data,
                             label_col = "label",
                             name_col = "NAME",
                             remove_pattern = " city$") {
  stopifnot(
    all(rlang::has_name(data, name_col)),
    !rlang::has_name(data, label_col)
  )

  data |>
    join_us_census_state() |>
    dplyr::mutate(
      "{label_col}" := stringr::str_remove(.data[[name_col]], remove_pattern),
      "{label_col}" := paste0(.data[[label_col]], ", ", STUSPS)
    )
}
