#' Helper function for labelling ggplot2 plots with the appropriate credit for NHGIS data
#'
#' @inheritDotParams ggplot2::labs
#' @keywords internal
#' @export
labs_nhgis <- function(...,
                       caption = NULL,
                       credit = "IPUMS NHGIS, University of Minnesota, www.nhgis.org.",
                       prefix = "Source: ",
                       width = 80) {
  check_installed("ggplot2")

  ggplot2::labs(
    ...,
    caption = stringr::str_wrap(
      paste0(c(caption, paste0(prefix, credit)), collapse = " "),
      width = width
    )
  )
}
