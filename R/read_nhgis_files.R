#' List files from IPUMS zip file download
#'
#' @noRd
list_file_select <- function(file = NULL,
                             type = "data") {
  # FIXME: This is duplicative w/ ipumsr::ipums_list_files
  files <- utils::unzip(file, list = TRUE)

  pattern <- switch(type,
    "data" = ".+csv$",
    "shape" = ".+(shp|zip)$"
  )

  files[grepl(pattern, files[["Name"]]), "Name"]
}

#' Read IPUMS geometry using `ipumsr::read_ipums_sf`
#'
#' @inheritParams ipumsr::read_ipums_sf
#' @export
#' @importFrom ipumsr read_ipums_sf
read_ipums_geometry <- function(shape_file = NULL,
                                path = NULL,
                                file_select = NULL,
                                vars = "GISJOIN",
                                encoding = NULL,
                                bind_multiple = TRUE,
                                add_layer_var = NULL,
                                verbose = FALSE) {
  if (is.null(shape_file) && has_name(path, "shape")) {
    shape_file <- path[["shape"]]
  }

  stopifnot(
    file.exists(shape_file)
  )

  file_select <- file_select %||%
    list_file_select(shape_file, "shape")

  ipumsr::read_ipums_sf(
    shape_file = shape_file,
    file_select = file_select,
    vars = vars,
    encoding = encoding,
    bind_multiple = bind_multiple,
    add_layer_var = add_layer_var,
    verbose = verbose
  )
}

#' Read NHGIS data using `ipumsr::read_nhgis`
#'
#' @inheritParams ipumsr::read_nhgis
#' @export
#' @importFrom vctrs vec_rbind
#' @importFrom ipumsr read_nhgis
read_nhgis_data <- function(data_file = NULL,
                            path = NULL,
                            file_select = NULL,
                            multiple = TRUE,
                            verbose = FALSE,
                            ...,
                            format = c("tidy", "wide"),
                            variable_col = "variable",
                            value_col = "value",
                            column_title_col = "column_title",
                            denominator_prefix = "denominator_",
                            variable_starts_with = c(
                              "A", "B", "D0", "AV",
                              "A4", "BS", "BUQ", "B0J",
                              "B7", "CV", "CM", "CL"
                            ),
                            perc_prefix = "perc_",
                            digits = 2) {
  if (is.null(data_file) && has_name(path, "data")) {
    data_file <- path[["data"]]
  }

  format <- arg_match(format, c("tidy", "wide"))

  stopifnot(
    file.exists(data_file)
  )

  file_select <- file_select %||%
    list_file_select(data_file, "data")

  nhgis_data <- lapply(
    rlang::set_names(file_select, file_select),
    \(file) {
      data <- ipumsr::read_nhgis(
        data_file = data_file,
        file_select = file,
        verbose = verbose,
        ...
      )

      if (format == "tidy") {
        data <- data |>
          pivot_nhgis_data(
            variable_col = variable_col,
            value_col = value_col,
            column_title_col = column_title_col,
            denominator_prefix = denominator_prefix,
            variable_starts_with = variable_starts_with
          ) |>
          join_nhgis_percent(
            variable_col = variable_col,
            value_col = value_col,
            column_title_col = column_title_col,
            denominator_prefix = denominator_prefix,
            perc_prefix = perc_prefix,
            digits = digits
          )
      }

      data
    }
  )

  vctrs::vec_rbind(
    !!!nhgis_data,
    .names_to = "filename"
  )
}

#' @rdname read_nhgis_data
#' @keywords internal
#' @export
read_nhgis_ext <- function(data_file,
                           file_select = NULL,
                           verbose = FALSE,
                           var_attrs = c("val_labels", "var_label", "var_desc"),
                           ...,
                           format = "tidy",
                           variable_col = "variable",
                           value_col = "value",
                           column_title_col = "column_title",
                           denominator_prefix = "denominator_",
                           variable_starts_with = c(
                             "A", "B", "D0", "AV",
                             "A4", "BS", "BUQ", "B0J",
                             "B7", "CV", "CM", "CL"
                           ),
                           perc_prefix = "perc_",
                           digits = 2) {
  format <- arg_match(format, c("tidy", "wide"))

  data <- ipumsr::read_nhgis(
    data_file = data_file,
    file_select = file,
    verbose = verbose,
    var_attrs = var_attrs,
    ...
  )

  if (format == "wide") {
    return(data)
  }

  data |>
    pivot_nhgis_data(
      variable_col = variable_col,
      value_col = value_col,
      column_title_col = column_title_col,
      denominator_prefix = denominator_prefix,
      variable_starts_with = variable_starts_with
    ) |>
    join_nhgis_percent(
      variable_col = variable_col,
      value_col = value_col,
      column_title_col = column_title_col,
      denominator_prefix = denominator_prefix,
      perc_prefix = perc_prefix,
      digits = digits
    )
}

#' Read NHGIS data and geometry
#'
#' Read NHGIS data and geometry to return a named list or a combined `sf`
#' object.
#'
#' @param path Optional if `data_file` is supplied. A named list with a "data"
#'   and "shape" element containing the paths to the `data_file` and
#'   `shape_file` arguments of used by [ipumsr::read_nhgis()] and
#'   [ipumsr::read_ipums_sf()].
#' @inheritParams read_nhgis_data
#' @inheritParams read_ipums_geometry
#' @param data_file_select,shape_file_select Passed to `file_select` parameter
#'   of [read_nhgis_data()] or [read_ipums_geometry()].
#' @returns A named list with "data" and "shape" elements or a combined sf data
#'   frame.
#' @export
#' @importFrom dplyr left_join join_by
read_nhgis_files <- function(path = NULL,
                             data_file = NULL,
                             data_file_select = NULL,
                             shape_file = NULL,
                             shape_file_select = NULL,
                             verbose = FALSE,
                             geometry = FALSE,
                             ...) {
  nhgis_data <- read_nhgis_data(
    path = path,
    data_file = data_file,
    file_select = data_file_select,
    verbose = verbose,
    ...
  )

  nhgis_shape <- NULL

  if (!is.null(shape_file) || (has_name(path, "shape") && !is.null(path[["shape"]]))) {
    nhgis_shape <- read_ipums_geometry(
      path = path,
      shape_file = shape_file,
      file_select = shape_file_select,
      ...
    )
  }

  if (!geometry) {
    if (!is.null(nhgis_shape)) {
      nhgis_data <- list(
        "data" = nhgis_data,
        "shape" = nhgis_shape
      )
    }

    return(nhgis_data)
  }

  check_installed("sf")

  # FIXME: Should this be replaced with a ipums_shape_join_* function?
  nhgis_data <- dplyr::left_join(
    nhgis_data,
    nhgis_shape,
    by = dplyr::join_by(GISJOIN)
  )

  sf::st_as_sf(nhgis_data)
}
