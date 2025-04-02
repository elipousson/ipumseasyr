#' Format NHGIS data from 1960 Census
#'
#' @inheritParams fmt_perc_value_col
#' @noRd
fmt_nhgis_1960 <- function(data,
                           variable_col = "variable",
                           column_title_col = "column_title",
                           value_col = "value",
                           denominator_prefix = "denominator_",
                           perc_prefix = "perc_",
                           join_cols = c("GISJOIN", "YEAR"),
                           digits = 2) {
  data <- data |>
    dplyr::mutate(
      # FIXME: These synthetic variable identifiers must be documented
      "{denominator_prefix}{variable_col}" := dplyr::case_when(
        # Population
        .data[[variable_col]] %in% c("B7B001", "B7B002", "B7B003") ~ "B7B00",
        # Total occupied units
        .data[[variable_col]] %in% c("B64001", "B64002", "B64003", "B64004") ~ "B6400",
        # Total occupied units
        .data[[variable_col]] %in% c("B83001", "B83002") ~ "B8300",
        # Total workers
        .data[[variable_col]] %in% c("B9D001", "B9D002", "B9D003", "B9D004") ~ "B9D00",
        # Commuting workers (reporting means of transportation)
        .data[[variable_col]] %in% c("B9G001", "B9G002", "B9G003", "B9G004", "B9G005", "B9G006") ~ "B9G00"
      )
    )

  denominator_data <- summarise_nhgis_1960_denominators(
    data = data,
    variable_col = variable_col,
    column_title_col = column_title_col,
    value_col = value_col,
    denominator_prefix = denominator_prefix,
    join_cols = join_cols
  )

  denom_variable_col <- paste0(denominator_prefix, variable_col)

  data |>
    dplyr::left_join(
      denominator_data,
      by = c(join_cols, denom_variable_col),
      na_matches = "never"
    ) |>
    fmt_perc_value_col(
      variable_col = variable_col,
      value_col = value_col,
      column_title_col = column_title_col,
      denominator_prefix = denominator_prefix,
      perc_prefix = perc_prefix,
      join_cols = join_cols,
      digits = digits
    )
}

#' @noRd
summarise_nhgis_1960_denominators <- function(data,
                                              variable_col = "variable",
                                              column_title_col = "column_title",
                                              value_col = "value",
                                              denominator_prefix = "denominator_",
                                              join_cols = c("GISJOIN", "YEAR")) {
  denom_variable_col <- paste0(denominator_prefix, variable_col)

  if (!has_name(data, denom_variable_col)) {
    data <- data |>
      dplyr::mutate(
        "{denominator_prefix}{variable_col}" := forcats::fct_collapse(
          .data[[variable_col]],
          # Population
          "B7B00" = c("B7B001", "B7B002", "B7B003"),
          # Total occupied units
          "B6400" = c("B64001", "B64002", "B64003", "B64004"),
          # Total occupied units
          "B8300" = c("B83001", "B83002"),
          # Total workers
          "B9D00" = c("B9D001", "B9D002", "B9D003", "B9D004"),
          # Commuting workers (reporting means of transportation)
          "B9G00" = c("B9G001", "B9G002", "B9G003", "B9G004", "B9G005", "B9G006")
        )
      )
  }

  data |>
    dplyr::filter(!is.na({{ denom_variable_col }})) |>
    dplyr::summarise(
      "{denominator_prefix}{value_col}" := sum(.data[[value_col]], na.rm = TRUE),
      .by = all_of(c(join_cols, denom_variable_col))
    ) |>
    dplyr::mutate(
      "{denominator_prefix}{column_title_col}" := dplyr::case_match(
        .data[[denom_variable_col]],
        "B7B00" ~ "Population",
        "B6400" ~ "Total occupied units",
        "B8300" ~ "Total occupied units",
        # Total workers
        "B9D00" ~ "Total workers",
        # Commuting workers (reporting means of transportation)
        "B9G00" ~ "Commuting workers"
      )
    )
}
