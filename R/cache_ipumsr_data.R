#' Get path to `{ipumseasyr}` package cache directory
#'
#' @keywords internal
#' @inheritParams base::dir.create
#' @export
#' @importFrom rappdirs user_cache_dir
ipumsr_cache_dir <- function(pkg = "ipumseasyr", recursive = TRUE) {
  cache_dir <- rappdirs::user_cache_dir(pkg)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = recursive)
  }
  cache_dir
}

#' Get cached object from `{ipumseasyr}` package cache
#'
#' [get_ipumsr_cache()] reads a cached IPUMS data file using [readr::read_rds()].
#'
#' @param file File name of the cached RDS file.
#' @param refresh If `FALSE`, do not read a file from cache. If `TRUE`, read a
#'   file from cache if it exists at the supplied path.
#' @param path Path to package cache directory. Defaults to [ipumsr_cache_dir()]
#' @keywords internal
#' @export
get_ipumsr_cache <- function(
  code,
  refresh = FALSE,
  file = NULL,
  path = ipumsr_cache_dir()
) {
  if (!refresh && file.exists(file.path(path, file))) {
    check_installed("readr")
    return(readr::read_rds(file.path(path, file)))
  }

  force(code)
}
