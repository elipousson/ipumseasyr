#' WIP: Function for pivoting NHGIS data to assign denominator variables
#'
#' @noRd
pivot_nhgis_data <- function(data) {
  check_installed("tidyr")

  data |>
    tidyr::pivot_longer(
      cols = starts_with(c("A", "B", "D")),
      names_to = "variable",
      values_to = "value"
    ) |>
    # Join labels from dictionary
    dplyr::left_join(
      nhgis_variables,
      by = "variable"
    ) |>
    dplyr::mutate(
      denominator_variable = dplyr::case_when(
        # Total units
        variable %in% c("A41AA", "A43AA", "A43AB") ~ "A41AA",
        # Occupied units
        variable %in% c("B37AA", "B37AB") ~ "A43AA",
        variable %in% c(
          "B18AA", "B18AB", "B18AC", "B18AD", "B18AE",
          "B57AA", "B57AB", "B57AC", "B57AD", "B57AE",
          "B57AF", "B57AG", "B57AH", "B57AI", "B57AJ",
          "B57AK", "B57AL", "B57AM", "B57AN", "B57AO",
          "B57AP", "B57AQ", "B57AR", "D08AA", "D08AB"
        ) ~ "AV0AA"
      )
    )
}
