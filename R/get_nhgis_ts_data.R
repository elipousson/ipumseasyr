#' List NHGIS time series tables using `ipumsr::get_metadata_nhgis`
#'
#' Use `ipumsr::get_metadata_nhgis()` with `type = "time_series_tables"` to
#' return a data frame of time series tables. Optionally filter by geographical
#' integration type "nominal" or "standardized" ("2010" or "standardized to
#' 2010" also work).
#'
#' @param ... Additional parameters passed to `ipumsr::get_metadata_nhgis()`
#' @param integration Optional filter for geographical integration.
#' @inheritParams get_ipumsr_cache
#' @export
list_nhgis_ts_tables <- function(...,
                                 cache = TRUE,
                                 cache_file = "nhgis_time_series_tables.rds",
                                 refresh = FALSE,
                                 integration = NULL) {
  cache_path <- file.path(
    ipumsr_cache_dir(), cache_file
  )

  tables <- get_ipumsr_cache(
    {
      ipumsr::get_metadata_nhgis(
        type = "time_series_tables",
        ...
      )
    },
    file = cache_file,
    path = ipumsr_cache_dir(),
    refresh = refresh
  )

  if (cache && (!file.exists(cache_path) || refresh)) {
    check_installed("readr")
    readr::write_rds(tables, file = cache_path)
  }

  if (!is.null(integration)) {
    integration <- switch(tolower(integration),
      "nominal" = "Nominal",
      "2010" = "Standardized to 2010",
      "standardized" = "Standardized to 2010",
      "standardized to 2010" = "Standardized to 2010"
    )

    tables <- tables[
      table[["geographic_integration"]] == integration,
    ]
  }

  tables
}

#' Define a NHGIS time series extract using `ipumsr::define_extract_nhgis`
#'
#' [define_nhgis_ts_extract()] is a wrapper for [ipumsr::define_extract_nhgis()]
#' with defaults that support the creation of tidy data using
#' [read_nhgis_data()] or [pivot_nhgis_data()].
#'
#' @inheritParams list_nhgis_shapefiles
#' @inheritParams ipumsr::define_extract_nhgis
#' @inheritParams ipumsr::get_metadata_nhgis
#' @param geometry If `TRUE`, include shapefiles in the defined extract. If
#'   shapefiles is `NULL`, the function uses [list_nhgis_shapefiles()] with
#'   `shape_year` as the `year` parameter.
#' @inheritDotParams ipumsr::define_extract_nhgis -tst_layout
#' @param output Used to set `tst_layout` value. c("tidy", "wide", "file")
#'   corresponding to "time_by_row_layout", "time_by_column_layout", or
#'   "time_by_file_layout".
#' @export
define_nhgis_ts_extract <- function(year = NULL,
                                    tables = NULL,
                                    geography = c("county", "state"),
                                    extent = "us",
                                    output = c("tidy", "wide", "file"),
                                    shape_year = NULL,
                                    basis = 2008,
                                    geometry = FALSE,
                                    ...,
                                    time_series_tables = NULL,
                                    description = NULL,
                                    shapefiles = NULL,
                                    data_format = "csv_no_header",
                                    validate = TRUE,
                                    api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (is.null(time_series_tables)) {
    if (validate) {
      tables <- rlang::arg_match(
        tables,
        values = nhgis_ts_tables,
        multiple = TRUE
      )
    }

    year <- as.character(year)

    time_series_tables <- lapply(
      tables,
      \(name) {
        if (validate) {
          tbl_metadata <- ipumsr::get_metadata_nhgis(
            time_series_table = name,
            api_key = api_key
          )

          year <- rlang::arg_match(
            year,
            tbl_metadata[["years"]][["name"]],
            multiple = TRUE
          )

          geography <- rlang::arg_match(
            geography,
            tbl_metadata[["geog_levels"]][["name"]],
            multiple = TRUE
          )
        }

        ipumsr::tst_spec(
          name = name,
          geog_levels = geography,
          years = year
        )
      }
    )
  }

  if (geometry) {
    shapefiles <- shapefiles %||% list_nhgis_shapefiles(
      geography = geography,
      year = shape_year %||% year,
      extent = extent,
      basis = basis,
      validate = validate,
      api_key = api_key
    )
  }

  output <- rlang::arg_match(output)

  tst_layout <- switch(output,
    "tidy" = "time_by_row_layout",
    "wide" = "time_by_column_layout",
    "file" = "time_by_file_layout"
  )

  ipumsr::define_extract_nhgis(
    description = nhigs_description(
      description = description,
      time_series_tables
    ),
    time_series_tables = time_series_tables,
    shapefiles = shapefiles,
    tst_layout = tst_layout,
    data_format = data_format,
    ...
  )
}

#' Helper function for creating a default NHGIS extract description
#' @noRd
nhigs_description <- function(description,
                              ts_tables = NULL) {
  if (!is.null(ts_tables)) {
    description %||% paste0(
      "Extract created with {ipumseasyr} R package ",
      "using the time series tables: ", paste0(ts_tables, collapse = ", "), "."
    )
  } else {
    description %||% "Extract created with the {ipumseasyr} R package."
  }
}

#' Get NHGIS time series data
#'
#' Use `define_nhgis_ts_extract()`, `ipumsr::submit_extract()`,
#' `ipumsr::download_extract()`, and `read_nhgis_files()` to define, submit,
#' download, and read a NHGIS time series extract. This function is *only*
#' recommended for interactive use and is *not* recommended if you are
#' requesting a large number of tables or geographies.
#'
#' @inheritParams define_nhgis_ts_extract
#' @inheritParams ipumsr::submit_extract
#' @inheritParams ipumsr::download_extract
#' @inheritParams read_nhgis_files
#' @export
get_nhgis_ts_data <- function(year = NULL,
                              tables = NULL,
                              geography = c("county", "state"),
                              extent = "us",
                              output = c("tidy", "wide", "file"),
                              basis = 2008,
                              shape_year = NULL,
                              geometry = FALSE,
                              extract = NULL,
                              data_file = NULL,
                              shape_file = NULL,
                              state = NULL,
                              ...,
                              time_series_tables = NULL,
                              description = NULL,
                              shapefiles = NULL,
                              data_format = "csv_no_header",
                              validate = TRUE,
                              submit_extract = TRUE,
                              download_extract = TRUE,
                              read_files = TRUE,
                              download_dir = getwd(),
                              overwrite = FALSE,
                              progress = TRUE,
                              verbose = progress,
                              api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (is.null(data_file) && is.null(extract)) {
    extract <- define_nhgis_ts_extract(
      year = year,
      tables = tables,
      geography = geography,
      shape_year = shape_year,
      extent = extent,
      output = output,
      basis = basis,
      geometry = geometry,
      ...,
      time_series_tables = time_series_tables,
      description = description,
      shapefiles = shapefiles,
      data_format = data_format,
      validate = validate,
      api_key = api_key
    )
  }

  extract_paths <- get_ipumsr_extract_paths(
    extract = extract,
    data_file = data_file,
    shape_file = shape_file,
    submit_extract = submit_extract,
    download_extract = download_extract,
    api_key = api_key
  )

  if (!read_files || !is_bare_list(extract_paths)) {
    # FIXME: Add warning if extract_paths is not a bare list
    return(extract_paths)
  }

  read_nhgis_files(
    path = extract_paths,
    verbose = verbose,
    geometry = geometry
  )
}
