#' Get path to `{ipumseasyr}` package cache directory
#' @importFrom rappdirs user_cache_dir
ipumsr_cache_dir <- function(pkg = "ipumseasyr") {
  cache_dir <- rappdirs::user_cache_dir(pkg)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  cache_dir
}

#' Get cached object from `{ipumseasyr}` package cache
#' @noRd
get_ipumsr_cache <- function(code,
                             refresh = FALSE,
                             file = NULL,
                             path = ipumsr_cache_dir()) {
  if (!refresh && file.exists(file.path(path, file))) {
    check_installed("readr")
    return(readr::read_rds(file.path(path, file)))
  }

  force(code)
}
