#' Pivot NHGIS data longer to assign denominator variables
#'
#' [pivot_nhgis_data()] switches NHGIS data from a wide to long format and
#' creates a denominator column based on a preset crosswalk between variables
#' and corresponding denominators.
#'
#' @param variable_col Variable column name
#' @param value_col Value column name
#' @param label_col Label column name
#' @keywords internal
#' @export
pivot_nhgis_data <- function(data,
                             variable_col = "variable",
                             value_col = "value",
                             column_title_col = "column_title",
                             denominator_prefix = "denominator_",
                             variable_starts_with = c(
                               "A", "B", "D0",
                               "AV", "A4",
                               "BS", "BUQ", "B0J", "B7",
                               "CV", "CM", "CL"
                             )) {
  check_installed(c("tidyr", "labelled", "tibble"))

  nhgis_var_labels <- data |>
    labelled::get_variable_labels()

  stopifnot(
    !is_empty(nhgis_var_labels)
  )

  nhgis_variables <- nhgis_var_labels |>
    tibble::enframe(
      name = variable_col,
      value = column_title_col
    ) |>
    dplyr::mutate(
      "{column_title_col}" := as.character(.data[[column_title_col]])
    )

  year_cols <- c("GEOGYEAR", "DATAYEAR", "YEAR")

  keep_cols <- c(
    "GISJOIN", "AREA", "NAME", "AREANAME",
    year_cols,
    "STATE", "STATEA", "STATEFP", "STATENH", "STATEICP",
    "COUNTY", "COUNTYA", "COUNTYFP", "COUNTYNH", "COUNTYICP",
    "PRETRACTA", "TRACTA", "POSTTRCTA"
  )

  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(year_cols),
        as.character
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(
        dplyr::starts_with(variable_starts_with),
        !dplyr::any_of(keep_cols)
      ),
      names_to = variable_col,
      values_to = value_col
    ) |>
    # Join labels from dictionary
    dplyr::left_join(
      nhgis_variables,
      by = variable_col
    ) |>
    dplyr::mutate(
      "{denominator_prefix}{variable_col}" := dplyr::case_when(
        # Totals
        .data[[variable_col]] %in% c(
          "A41AA", "A68AA", "AV0AA", "A00AA", "AR5AA",
          "CM7AA", "CL8AA"
        ) ~ .data[[variable_col]],

        # Total units
        .data[[variable_col]] %in% c("A43AA", "A43AB") ~ "A41AA",

        # Occupied units
        .data[[variable_col]] %in% c("B37AA", "B37AB") ~ "A43AA",

        # Families
        .data[[variable_col]] %in% c(
          "A88AA", "A88AB", "A88AC", "A88AD", "A88AE"
        ) ~ "A68AA",

        # Persons
        .data[[variable_col]] %in% c(
          "A35AA",
          "B18AA", "B18AB", "B18AC", "B18AD", "B18AE",
          "B57AA", "B57AB", "B57AC", "B57AD", "B57AE",
          "B57AF", "B57AG", "B57AH", "B57AI", "B57AJ",
          "B57AK", "B57AL", "B57AM", "B57AN", "B57AO",
          "B57AP", "B57AQ", "B57AR", "D08AA", "D08AB",
          "B14AA", "B14AB"
        ) ~ "AV0AA",

        # Negro
        .data[[variable_col]] %in% c("BYA003", "A8L005", "A8L006") ~ "BYA003",

        # Households
        .data[[variable_col]] %in% c("CM9AA", "CM9AB") ~ "CM7AA",

        # Occupied housing units
        .data[[variable_col]] %in% c("CV5AA", "CV5AB", "CV5AC",
                                     "CV5AD", "CV5AE", "CV5AF") ~ "A43AA",
        # Housing units
        # .data[[variable_col]] %in% c("CM9AA", "CM9AB") ~ "CM7AA",

        .data[[variable_col]] %in% c("AF15001", "AF15002") ~ "AV0AA",

        # White
        .data[[variable_col]] %in% c("A8L001", "A8L002") ~ "AF15001",

        # White Native-born / White Foreign-born use White denominator
        .data[[variable_col]] %in% c("BYA001", "BYA002") ~ "AF15001",

        .data[[variable_col]] %in% c("BS7AA", "BS7AB", "BS7AC", "BS7AD") ~ "AR5AA"
      ),
      .after = dplyr::all_of(variable_col)
    )

  moe_join <- any(c(
    stringr::str_detect(data[[column_title_col]], "^(Lower bound|Upper bound|Margin of error)")
  ))

  if (!moe_join) {
    return(data)
  }

  data |>
    join_moe_cols(
      moe_pattern = "^Lower bound",
      moe_variable_remove = "L$",
      moe_col = paste0(value_col, "_lower"),
      variable_col = variable_col,
      column_title_col = column_title_col,
      value_col = value_col,
      denominator_prefix = denominator_prefix
    ) |>
    join_moe_cols(
      moe_pattern = "^Upper bound",
      moe_variable_remove = "U$",
      moe_col = paste0(value_col, "_upper"),
      variable_col = variable_col,
      column_title_col = column_title_col,
      value_col = value_col,
      denominator_prefix = denominator_prefix
    ) |>
    join_moe_cols(
      moe_pattern = "^Margin of error",
      moe_variable_remove = "M$",
      moe_col = "moe",
      variable_col = variable_col,
      column_title_col = column_title_col,
      value_col = value_col,
      denominator_prefix = denominator_prefix
    )
}

#' @noRd
join_moe_cols <- function(
    data,
    moe_pattern = "^Lower bound",
    moe_variable_remove = "L$",
    variable_col = "variable",
    column_title_col = "column_title",
    value_col = "value",
    moe_col = "value_lower",
    denominator_prefix = "denominator_") {
  moe_pattern_match <- stringr::str_detect(
    data[[column_title_col]],
    moe_pattern
  )

  if (!any(moe_pattern_match)) {
    return(data)
  }

  moe_join_data <- data |>
    dplyr::filter(moe_pattern_match) |>
    dplyr::rename(
      dplyr::any_of(set_names(value_col, moe_col))
    ) |>
    dplyr::mutate(
      "{variable_col}" := stringr::str_remove(
        .data[[variable_col]],
        moe_variable_remove
      )
    ) |>
    dplyr::select(
      !dplyr::any_of(column_title_col),
      !dplyr::starts_with(denominator_prefix)
    )

  if (inherits(moe_join_data, "sf")) {
    check_installed("sf")
    moe_join_data <- sf::st_drop_geometry(moe_join_data)
  }

  data |>
    dplyr::filter(!moe_pattern_match) |>
    dplyr::left_join(
      moe_join_data,
      na_matches = "never"
    ) |>
    suppressMessages() |>
    dplyr::relocate(
      dplyr::all_of(moe_col),
      .after = dplyr::all_of(value_col)
    )
}

#' @rdname pivot_nhgis_data
#' @param perc_prefix Prefix string to use for calculated percent value.
#' @name join_nhgis_percent
#' @keywords internal
#' @export
join_nhgis_percent <- function(data,
                               variable_col = "variable",
                               value_col = "value",
                               column_title_col = "column_title",
                               denominator_prefix = "denominator_",
                               perc_prefix = "perc_",
                               digits = 2) {
  denom_variable_col <- paste0(denominator_prefix, variable_col)

  denom_value_col <- paste0(denominator_prefix, value_col)

  denom_title_col <- paste0(denominator_prefix, column_title_col)

  denominator_data <- data |>
    dplyr::filter(
      .data[[variable_col]] %in% data[[denom_variable_col]]
    ) |>
    dplyr::select(!all_of(denom_variable_col)) |>
    dplyr::rename(
      dplyr::all_of(
        rlang::set_names(
          c(variable_col, value_col, column_title_col),
          c(denom_variable_col, denom_value_col, denom_title_col)
        )
      )
    )

  data |>
    dplyr::left_join(
      denominator_data
    ) |>
    dplyr::mutate(
      "{perc_prefix}{value_col}" := dplyr::if_else(
        !is.na(.data[[denom_variable_col]]) & !is.na(.data[[value_col]]) & .data[[denom_value_col]] > 0,
        round(.data[[value_col]] / .data[[denom_value_col]], digits = digits),
        NA_real_
      ),
      .after = dplyr::starts_with(value_col)
    )
}
