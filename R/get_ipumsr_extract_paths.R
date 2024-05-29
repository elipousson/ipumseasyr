#' Download IPUMS extract using `ipumsr::wait_for_extract` and
#' `ipumsr::download_extract`
#'
#' [download_ipumsr_extract()] is a wrapper for [ipumsr::wait_for_extract()] and
#' [ipumsr::download_extract()] to wait until an extract is ready for download
#' before attempting to download it.
#'
#' @inheritParams ipumsr::wait_for_extract
#' @inheritParams ipumsr::download_extract
#' @inheritDotParams ipumsr::wait_for_extract
#' @importFrom ipumsr wait_for_extract download_extract
#' @export
download_ipumsr_extract <- function(extract = NULL,
                                    download_dir = getwd(),
                                    overwrite = FALSE,
                                    progress = TRUE,
                                    ...,
                                    api_key = Sys.getenv("IPUMS_API_KEY")) {
  ipumsr::wait_for_extract(
    extract = extract,
    api_key = api_key,
    ...
  )

  ipumsr::download_extract(
    extract = extract,
    download_dir = download_dir,
    overwrite = overwrite,
    progress = progress,
    api_key = api_key
  )
}

#' Get extract paths for extract with optional support for cached extract files
#'
#' Download extract with [download_ipumsr_extract()] and return a list of file
#' paths for the data and shape files.
#'
#' @param submit_extract If `extract` is not `NULL` and `submit_extract = TRUE`,
#'   use [ipumsr::submit_extract] to submit the extract.
#' @inheritParams ipumsr::submit_extract
#' @inheritParams download_ipumsr_extract
#' @returns A named list with "data" and "shape" elements containing extract file paths.
#' @export
#' @importFrom ipumsr submit_extract
get_ipumsr_extract_paths <- function(extract = NULL,
                                     data_file = NULL,
                                     shape_file = NULL,
                                     submit_extract = TRUE,
                                     download_extract = TRUE,
                                     download_dir = getwd(),
                                     overwrite = FALSE,
                                     progress = TRUE,
                                     refresh = FALSE,
                                     api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (!is.null(extract)) {
    # Check cache here
    if (submit_extract) {
      extract <- ipumsr::submit_extract(extract)
    }

    # FIXME: This is a start on a extract caching function
    path <- get_ipumsr_cache(
      {
        download_ipumsr_extract(
          extract = extract,
          download_dir = download_dir,
          overwrite = overwrite,
          progress = progress,
          api_key = api_key
        )
      },
      file = paste0(
        gsub("/|\\.|:", "", extract[["download_links"]][["table_data"]][["url"]]),
        ".rds"
      ),
      path = ipumsr_cache_dir(),
      refresh = refresh
    )

    return(path)
  }

  stopifnot(!is.null(data_file) || !is.null(shape_file))

  list(
    "data" = data_file,
    "shape" = shape_file
  )
}
