% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nhgis_ts_data.R
\name{list_nhgis_metadata}
\alias{list_nhgis_metadata}
\title{List NHGIS metadata}
\usage{
list_nhgis_metadata(
  ...,
  cache = FALSE,
  cache_file = "nhgis_time_series_tables.rds",
  refresh = FALSE,
  integration = NULL
)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[ipumsr:get_metadata_nhgis]{ipumsr::get_metadata_nhgis}}
  \describe{
    \item{\code{dataset}}{Name of an individual dataset for which to retrieve metadata.}
    \item{\code{data_table}}{Name of an individual data table for which to retrieve
metadata. If provided, an associated \code{dataset} must also be specified.}
    \item{\code{time_series_table}}{Name of an individual time series table for which
to retrieve metadata.}
    \item{\code{delay}}{Number of seconds to delay between
successive API requests, if multiple requests are needed to retrieve all
records.

A delay is highly unlikely to be necessary and is intended only as a
fallback in the event that you cannot retrieve all metadata records without
exceeding the API rate limit.

Only used if \code{type} is provided.}
    \item{\code{api_key}}{API key associated with your user account. Defaults to the
value of the \code{IPUMS_API_KEY} environment variable. See
\code{\link[ipumsr:set_ipums_api_key]{set_ipums_api_key()}}.}
  }}

\item{cache}{If \code{TRUE}, cache metadata to file.}

\item{cache_file}{Default file name to use for cached metadata.}

\item{refresh}{If \code{TRUE}, force a refresh of the cached metadata file.}

\item{integration}{Optionally filter to "Standardized to 2010" or "Nominal" geographic integration. Values of "2010" or "standardized" are treated as "Standardized to 2010".}
}
\description{
\code{\link[=list_nhgis_metadata]{list_nhgis_metadata()}} uses \code{ipumsr::get_metadata_nhgis} to return and
(optionally) cache a data frame with metadata for NHGIS tables.
}
\keyword{internal}
