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
#'  @inheritParams ipumsr::read_ipums_sf
#'  @export
#'  @importFrom ipumsr read_ipums_sf
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
#'  @inheritParams ipumsr::read_nhgis
#'  @export
#'  @importFrom vctrs vec_rbind
#'  @importFrom ipumsr read_nhgis
read_nhgis_data <- function(data_file = NULL,
                            path = NULL,
                            file_select = NULL,
                            verbose = FALSE,
                            ...) {
  if (is.null(data_file) && has_name(path, "data")) {
    data_file <- path[["data"]]
  }

  stopifnot(
    file.exists(data_file)
  )

  file_select <- file_select %||%
    list_file_select(data_file, "data")

  nhgis_data <- lapply(
    rlang::set_names(file_select, file_select),
    \(file) {
      ipumsr::read_nhgis(
        data_file = data_file,
        file_select = file,
        verbose = verbose,
        ...
      )
    }
  )

  vctrs::vec_rbind(
    !!!nhgis_data,
    .names_to = "filename"
  )
}

#' Read NHGIS data and geometry
#'
#'  @inheritParams ipumsr::read_nhgis
#'  @export
#'  @importFrom dplyr left_join join_by
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

  nhgis_data <- dplyr::left_join(
    nhgis_data,
    nhgis_shape,
    by = dplyr::join_by(GISJOIN)
  )

  sf::st_as_sf(nhgis_data)
}
