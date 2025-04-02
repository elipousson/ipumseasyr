#' Pivot NHGIS data longer to assign denominator variables and join percent
#' values
#'
#' @description
#' [pivot_nhgis_data()] uses [tidyr::pivot_longer()] switches NHGIS data from a
#' wide to long format and creates a denominator column based on a preset
#' crosswalk between variables and corresponding denominators. The input data
#' must have variable column labels present.
#'
#' [join_nhgis_percent()] uses the denominator values added in pivoting to
#' calculating a percent share value. This feature is supported for many but not
#' all of the most popular NHGIS time series variables. Both function uses a
#' similar set of conventions as the `{getACS}` package to support ease of code
#' reuse between NHGIS and American Community Survey (ACS) data.
#'
#' At present, all numeric columns that do not appear to be an identifier
#' are pivoted.
#'
#' @param variable_col Variable column name
#' @param value_col Value column name
#' @param column_title_col Column title column name (to be created from column
#'   labels)
#' @inheritParams tidyr::pivot_longer
#' @param denominators Named list of denominator values.
#' @inheritParams cli::cli_abort
#' @seealso [join_nhgis_percent_change()]
#' @export
pivot_nhgis_data <- function(
  data,
  variable_col = "variable",
  value_col = "value",
  column_title_col = "column_title",
  denominator_prefix = "denominator_",
  cols_vary = "slowest",
  denominators = list(
    persons = "A00AA",
    families = "A68AA",
    housing_units = "A41AA",
    occupied_units = "A43AA"
  ),
  call = caller_env()
) {
  check_installed(c("tidyr", "labelled", "tibble"))

  year_cols <- c("GEOGYEAR", "DATAYEAR", "YEAR")

  if (!any(has_name(data, year_cols))) {
    cli_warn(
      c(
        "{.arg data} may be using a wide format with years in separate columns
        which is not recommended for this function.",
        "i" = 'Did you forget to set {.code tst_layout = "time_by_row_layout"}
      when defining the extract?'
      )
    )
  }

  if (has_name(data, variable_col)) {
    cli_abort(
      c(
        "{.arg data} already contains the {.arg variable_col} {.str {variable_col}}.",
        "i" = "If you used {.fn get_nhgis_ts_data} or {.fn read_nhgis_data} to
      access this data, the data may already be converted into a long format."
      ),
      call = call
    )
  }

  keep_cols <- c(
    "GISJOIN",
    "AREA",
    "NAME",
    "AREANAME",
    "NHGISCODE",
    year_cols,
    "STATE",
    "STATEA",
    "STATEFP",
    "STATENH",
    "STATEICP",
    "COUNTY",
    "COUNTYA",
    "COUNTYFP",
    "COUNTYNH",
    "COUNTYICP",
    "MSA",
    "PLACE",
    "PLACEDC",
    "PLACDESC",
    "URBAN",
    "60MCD",
    "60PLACESC",
    "CMSA",
    "CENCNTY",
    "CBD",
    "SEA",
    "UATYPE",
    "STUSAB",
    "DIVIS",
    "REG",
    "PRETRACTA",
    "TRACTA",
    "POSTTRCTA",
    "GNOTES",
    "geometry"
  )

  match_cols <- !(names(data) %in% keep_cols) &
    as.character(lapply(data, \(x) {
      class(x)[1]
    })) %in%
      c(
        "numeric",
        "logical" # NA values
      )

  match_variable_cols <- names(data)[match_cols]

  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(year_cols),
        as.character
      )
    )

  if (length(match_variable_cols) == 0) {
    cli::cli_warn("No pivot columns identified.")
    return(data)
  }

  nhgis_var_labels <- labelled::get_variable_labels(data)

  data <- data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(match_variable_cols),
      cols_vary = cols_vary,
      names_to = variable_col,
      values_to = value_col
    )

  has_any_denom_variables <- any(
    unique(data[[variable_col]]) %in%
      c(
        as.character(denominators),
        "A41AA",
        "A68AA",
        "AV0AA",
        "A00AA",
        "AR5AA",
        "CM7AA",
        "CL8AA",
        "BYA003",
        "CM7AA",
        "AF15001"
      )
  )

  if (has_any_denom_variables) {
    data <- data |>
      dplyr::mutate(
        "{denominator_prefix}{variable_col}" := dplyr::case_when(
          # Totals
          .data[[variable_col]] %in%
            c(
              "A41AA",
              "A68AA",
              "AV0AA",
              "A00AA",
              "AR5AA",
              "CM7AA",
              "CL8AA",
              as.character(denominators)
            ) ~
            .data[[variable_col]],

          # Total units
          .data[[variable_col]] %in%
            c(
              "A43AA",
              "A43AB"
            ) ~
            denominators[["housing_units"]], # ~ "A41AA",

          # Occupied units
          .data[[variable_col]] %in%
            c(
              "B37AA",
              "B37AB"
            ) ~
            denominators[["occupied_units"]], # ~ "A43AA",

          # Families
          .data[[variable_col]] %in%
            c(
              "A88AA",
              "A88AB",
              "A88AC",
              "A88AD",
              "A88AE"
            ) ~
            denominators[["families"]], # "A68AA",

          # Persons
          .data[[variable_col]] %in%
            c(
              "A35AA",
              "B18AA",
              "B18AB",
              "B18AC",
              "B18AD",
              "B18AE",
              "B57AA",
              "B57AB",
              "B57AC",
              "B57AD",
              "B57AE",
              "B57AF",
              "B57AG",
              "B57AH",
              "B57AI",
              "B57AJ",
              "B57AK",
              "B57AL",
              "B57AM",
              "B57AN",
              "B57AO",
              "B57AP",
              "B57AQ",
              "B57AR",
              "D08AA",
              "D08AB",
              "B14AA",
              "B14AB"
            ) ~
            denominators[["persons"]], # "AV0AA",

          # Persons
          # Persons: Not Hispanic or Latino and Persons: Hispanic or Latino
          .data[[variable_col]] %in%
            c(
              "CV4AA",
              "CV4AB",
              "CV4AC",
              "CV4AD",
              "CV4AE",
              "CV4AF",
              "CV4AG",
              "CV4AH",
              "CV4AI",
              "CV4AJ"
            ) ~
            denominators[["persons"]],

          # Negro
          .data[[variable_col]] %in% c("BYA003", "A8L005", "A8L006") ~ "BYA003",

          # Households
          .data[[variable_col]] %in% c("CM9AA", "CM9AB") ~ "CM7AA",

          # Occupied housing units
          .data[[variable_col]] %in%
            c(
              "CV5AA",
              "CV5AB",
              "CV5AC",
              "CV5AD",
              "CV5AE",
              "CV5AF"
            ) ~
            denominators[["occupied_units"]], # "A43AA",
          # Housing units
          # .data[[variable_col]] %in% c("CM9AA", "CM9AB") ~ "CM7AA",

          .data[[variable_col]] %in% c("AF15001", "AF15002") ~ "AV0AA",

          # White
          .data[[variable_col]] %in%
            c(
              "A8L001",
              "A8L002",
              # White Native-born / White Foreign-born use White denominator
              "BYA001",
              "BYA002"
            ) ~
            "AF15001",
          .data[[variable_col]] %in% c("BS7AA", "BS7AB", "BS7AC", "BS7AD") ~
            "AR5AA"
        ),
        .after = dplyr::all_of(variable_col)
      )
  }

  if (is_empty(nhgis_var_labels)) {
    cli::cli_warn("Assigning variable titles requires labelled columns")
    return(data)
  }

  nhgis_variables <- nhgis_var_labels |>
    tibble::enframe(
      name = variable_col,
      value = column_title_col
    ) |>
    dplyr::mutate(
      "{column_title_col}" := as.character(.data[[column_title_col]])
    )

  # Join labels from dictionary
  data <- data |>
    dplyr::left_join(
      nhgis_variables,
      by = variable_col
    )

  moe_len <- nchar(data[[variable_col]]) > 5 & !is.na(data[[variable_col]])

  moe_vars <- moe_len & stringr::str_detect(data[[variable_col]], "(M|U|L)$")

  if (!any(moe_vars)) {
    return(data)
  }

  nhgis_moe_cols <- c(
    paste0(value_col, "_lower"),
    paste0(value_col, "_upper"),
    "moe"
  )

  data <- data |>
    join_moe_cols(
      # moe_i = moe_len & stringr::str_detect(data[[variable_col]], "L$"),
      moe_pattern = "^Lower bound",
      moe_variable_remove = "L$",
      moe_col = nhgis_moe_cols[[1]],
      variable_col = variable_col,
      column_title_col = column_title_col,
      value_col = value_col,
      denominator_prefix = denominator_prefix,
      drop_cols = nhgis_moe_cols[2:3]
    ) |>
    join_moe_cols(
      # moe_i = moe_len & stringr::str_detect(data[[variable_col]], "U$"),
      moe_pattern = "^Upper bound",
      moe_variable_remove = "U$",
      moe_col = nhgis_moe_cols[[2]],
      variable_col = variable_col,
      column_title_col = column_title_col,
      value_col = value_col,
      denominator_prefix = denominator_prefix,
      drop_cols = nhgis_moe_cols[c(1, 3)]
    ) |>
    join_moe_cols(
      # moe_i = moe_len & stringr::str_detect(data[[variable_col]], "M$"),
      moe_pattern = "^Margin of error",
      moe_variable_remove = "M$",
      moe_col = nhgis_moe_cols[[3]],
      variable_col = variable_col,
      column_title_col = column_title_col,
      value_col = value_col,
      denominator_prefix = denominator_prefix,
      drop_cols = nhgis_moe_cols[1:2]
    )

  data
}

#' @noRd
join_moe_cols <- function(
  data,
  moe_pattern = "^Lower bound",
  # moe_i = NULL,
  moe_variable_remove = "L$",
  variable_col = "variable",
  column_title_col = "column_title",
  value_col = "value",
  moe_col = "value_lower",
  denominator_prefix = "denominator_",
  drop_cols = NULL
) {
  if (!has_name(data, column_title_col)) {
    return(data)
  }

  moe_pattern_match <- stringr::str_detect(
    data[[column_title_col]],
    moe_pattern
  )

  # if (is.null(moe_i) || !any(moe_i)) {
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

  if (!is.null(drop_cols)) {
    moe_join_data <- moe_join_data |>
      dplyr::select(!dplyr::any_of(drop_cols))
  }

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
join_nhgis_percent <- function(
  data,
  variable_col = "variable",
  value_col = "value",
  column_title_col = "column_title",
  denominator_prefix = "denominator_",
  perc_prefix = "perc_",
  join_cols = c("GISJOIN", "YEAR"),
  digits = 2
) {
  denom_variable_col <- paste0(denominator_prefix, variable_col)

  required_nm <- c(variable_col, value_col, denom_variable_col)
  missing_nm <- is.na(match(required_nm, names(data)))
  if (any(missing_nm)) {
    cli::cli_alert_warning(
      "Can't join percent values without the required column{?s}:
      {.field {required_nm[missing_nm]}}"
    )

    return(data)
  }

  denom_value_col <- paste0(denominator_prefix, value_col)

  denom_title_col <- paste0(denominator_prefix, column_title_col)

  denom_names <- set_names(
    c(variable_col, value_col, column_title_col),
    c(denom_variable_col, denom_value_col, denom_title_col)
  )

  denominator_data <- data |>
    dplyr::filter(.data[[variable_col]] %in% data[[denom_variable_col]]) |>
    dplyr::select(!all_of(denom_variable_col)) |>
    dplyr::rename(dplyr::all_of(denom_names)) |>
    dplyr::select(dplyr::any_of(c(join_cols, names(denom_names))))

  data |>
    fmt_perc_value_col(
      # TODO: Add an explicit join by that works in all cases
      denominator_data = denominator_data,
      value_col = value_col,
      denominator_prefix = denominator_prefix,
      perc_prefix = perc_prefix,
      digits = digits
    )
}

#' Join a percent change in variable relative to a reference year
#'
#' [join_nhgis_percent_change()] joins a percent change column relative to a
#' reference year. Optionally join a rank from the reference year using
#' [dplyr::ntile()].
#'
#' @param reference_year Reference year to use when calculating a percent change
#'   column.
#' @param rank,rank_n Passed to `x` and `n` arguments of [dplyr::ntile()] to
#'   join a reference rank value.
#' @param rank_by Used as `.by` argument of [dplyr::mutate()] if `rank_n` is not
#'   `NULL`.
#' @export
#' @importFrom dplyr ntile
join_nhgis_percent_change <- function(
  data,
  reference_year = NULL,
  value_col = "value",
  reference_prefix = "reference_",
  variable_col = "variable",
  year_col = "YEAR",
  rank_col = "rank",
  rank = NULL,
  rank_n = NULL,
  rank_by = NULL,
  ...,
  perc_prefix = "perc_change_",
  digits = 2
) {
  reference_year <- reference_year %||% min(as.integer(data[[year_col]]))

  stopifnot(
    has_length(reference_year, 1)
  )

  ref_value_col <- paste0(reference_prefix, tolower(value_col))
  ref_year_col <- paste0(reference_prefix, tolower(year_col))

  reference_data <- data |>
    dplyr::filter(
      .data[[year_col]] == reference_year
    ) |>
    dplyr::mutate(
      "{ref_value_col}" := .data[[value_col]],
      "{ref_year_col}" := .data[[year_col]]
    ) |>
    dplyr::select(
      all_of(
        c(
          ref_value_col,
          ref_year_col,
          "NHGISCODE",
          variable_col
        )
      )
    )

  if (!is.null(rank_n)) {
    reference_data <- reference_data |>
      dplyr::mutate(
        "{reference_prefix}{rank_col}" := dplyr::ntile(
          rank %||% dplyr::row_number(),
          n = rank_n
        ),
        .by = rank_by
      )
  }

  data |>
    fmt_perc_value_col(
      value_col = value_col,
      denominator_prefix = reference_prefix,
      denominator_data = reference_data,
      perc_prefix = perc_prefix,
      digits = digits
    )
}


#' Format a percent value column with rounding and handling for 0 and NA values
#'
#' @noRd
fmt_perc_value_col <- function(
  data,
  value_col = "value",
  denominator_prefix = "denominator_",
  denominator_data = NULL,
  ...,
  perc_prefix = "perc_",
  digits = 2
) {
  if (is.data.frame(denominator_data)) {
    if (nrow(denominator_data) == 0) {
      return(data)
    }

    data <- suppressMessages(
      dplyr::left_join(
        x = data,
        y = denominator_data,
        ...
      )
    )
  }

  denom_value_col <- paste0(denominator_prefix, value_col)
  perc_value_col <- paste0(perc_prefix, value_col)

  stopifnot(
    # TODO: Warn if data has a column w/ name matching `perc_value_col`
    # !has_name(data, perc_value_col),
    all(has_name(data, c(value_col, denom_value_col)))
  )

  data |>
    dplyr::mutate(
      "{perc_value_col}" := dplyr::case_when(
        is.na(.data[[denom_value_col]]) ~ NA_real_,
        is.na(.data[[value_col]]) ~ NA_real_,
        .data[[denom_value_col]] == 0 ~ NA_real_,
        .default = round(
          .data[[value_col]] / .data[[denom_value_col]],
          digits = digits
        )
      ),
      .after = dplyr::starts_with(value_col)
    )
}

#' Summarise data using a summary function
#'
#' In some cases, a denominator value can be created by summarizing across a set
#' of variables for a GISJOIN and year value. If a denominator variable is not
#' already identified it may need to be calculated and joined with
#' [fmt_perc_value_col()] setting `drop_existing = TRUE` before using this function
#'
#' @param data Input data w/ column names including `value_col` and any columns specified in `.by`.
#' @param .f Function to apply to column specified by `value_col`.
#' @param .by Join columns used by `dplyr::summarise`
#' @export
#' @keywords internal
summarise_denominator <- function(
  data,
  .by = c("GISJOIN", "YEAR"),
  .f = \(x) {
    sum(x, na.rm = TRUE)
  },
  denominator_column_title_value = "Total",
  variable_col = "variable",
  value_col = "value",
  column_title_col = "column_title",
  denominator_prefix = "denominator_"
) {
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }

  dplyr::summarise(
    data,
    "{denominator_prefix}{value_col}" := .f(.data[[value_col]]),
    "{denominator_prefix}{column_title_col}" := denominator_column_title_value,
    .by = dplyr::all_of(.by)
  )
}
