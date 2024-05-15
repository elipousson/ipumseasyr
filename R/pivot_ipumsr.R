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
                             label_col = "label",
                             denominator_prefix = "denominator_",
                             variable_starts_with = c(
                               "A", "B", "D0",
                               "AV", "A4",
                               "BS", "BUQ", "B0J", "B7",
                               "CV", "CM", "CL"
                             )) {
  check_installed(c("tidyr", "labelled", "tibble"))

  nhgis_variables <- data |>
    labelled::get_variable_labels() |>
    tibble::enframe(
      name = variable_col,
      value = label_col
    ) |>
    dplyr::mutate(
      "{label_col}" := as.character(.data[[label_col]])
    )

  data |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with(variable_starts_with),
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
        .data[[variable_col]] %in% c("A41AA", "AV0AA", "A68AA") ~ .data[[variable_col]],
        # Total units
        .data[[variable_col]] %in% c("A43AA", "A43AB") ~ "A41AA",
        # Occupied units
        .data[[variable_col]] %in% c("B37AA", "B37AB") ~ "A43AA",
        # Persons
        .data[[variable_col]] %in% c(
          "B18AA", "B18AB", "B18AC", "B18AD", "B18AE",
          "B57AA", "B57AB", "B57AC", "B57AD", "B57AE",
          "B57AF", "B57AG", "B57AH", "B57AI", "B57AJ",
          "B57AK", "B57AL", "B57AM", "B57AN", "B57AO",
          "B57AP", "B57AQ", "B57AR", "D08AA", "D08AB",
          "B14AA", "B14AB"
        ) ~ "AV0AA",
        # Households
        .data[[variable_col]] %in% c(
          "BS7AA", "BS7AB", "BS7AC", "BS7AD",
          "CV5AA", "CV5AB", "CV5AC", "CV5AD", "CV5AE", "CV5AF"
        ) ~ "A43AA"
      )
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
                               label_col = "label",
                               denominator_prefix = "denominator_",
                               perc_prefix = "perc_") {
  denom_variable_col <- paste0(denominator_prefix, variable_col)

  denom_value_col <- paste0(denominator_prefix, value_col)

  denom_label_col <- paste0(denominator_prefix, label_col)

  denominator_data <- data |>
    dplyr::filter(
      .data[[variable_col]] %in% data[[denom_variable_col]]
    ) |>
    dplyr::select(!all_of(denom_variable_col)) |>
    dplyr::rename(
      dplyr::all_of(
        rlang::set_names(
          c(variable_col, value_col, label_col),
          c(denom_variable_col, denom_value_col, denom_label_col)
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
        round(.data[[value_col]] / .data[[denom_value_col]], digits = 2),
        NA_real_
      )
    )
}
