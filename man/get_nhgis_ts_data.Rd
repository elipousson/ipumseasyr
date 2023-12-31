% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nhgis_ts_data.R
\name{get_nhgis_ts_data}
\alias{get_nhgis_ts_data}
\title{Get NHGIS time series data}
\usage{
get_nhgis_ts_data(
  year = NULL,
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
  read_download = TRUE,
  download_dir = getwd(),
  overwrite = FALSE,
  progress = TRUE,
  verbose = progress,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
}
\arguments{
\item{output}{Used to set \code{tst_layout} value. c("tidy", "wide", "file")}

\item{extract}{If not \code{NULL}, extract is returned as is.}

\item{download_dir}{Path to the directory where the files should be written.
Defaults to current working directory.}

\item{overwrite}{If \code{TRUE}, overwrite any conflicting files that
already exist in \code{download_dir}. Defaults to \code{FALSE}.}

\item{progress}{If \code{TRUE}, output progress bar showing the status of the
download request. Defaults to \code{TRUE}.}

\item{api_key}{API key associated with your user account. Defaults to the
value of the \code{IPUMS_API_KEY} environment variable. See
\code{\link[ipumsr:set_ipums_api_key]{set_ipums_api_key()}}.}
}
\description{
Use \code{define_nhgis_ts_extract()}, \code{ipumsr::submit_extract()},
\code{ipumsr::download_extract()}, and \code{read_nhgis_files()} to define, submit,
download, and read a NHGIS time series extract.
}
