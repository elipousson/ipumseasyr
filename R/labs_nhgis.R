#' Label ggplot2 plots with the appropriate credit caption for NHGIS data
#'
#' [labs_nhgis()] adds a standard credit caption for NHGIS data to make consistent attribution easier.
#'
#' @param credit Credit line for IPUMS.
#' @param width Maximum width of caption line passed to [stringr::str_wrap()].
#' @inheritDotParams ggplot2::labs
#' @param collapse String to collapse caption and credit. Defaults to `" "`. Set
#'   to `"\n"` to place the credit line on a separate line following the
#'   caption. Ignored if caption is `NULL`.
#' @keywords ggplot2
#' @export
#' @importFrom stringr str_wrap
labs_nhgis <- function(...,
                       caption = NULL,
                       credit = "IPUMS NHGIS, University of Minnesota, www.nhgis.org.",
                       prefix = "Source: ",
                       collapse = " ",
                       width = 80) {
  labs_credit(
    ...,
    caption = caption,
    credit = credit,
    prefix = prefix,
    collapse = collapse,
    width = width
  )
}

#' Helper for creating ggplot2 labels with a credit line in the caption
#' @noRd
labs_credit <- function(...,
                        caption = NULL,
                        credit = NULL,
                        prefix = "Source: ",
                        collapse = " ",
                        width = 80,
                        call = caller_env()) {
  check_installed("ggplot2", call = call)

  if (!is.null(credit)) {
    credit <- paste0(prefix, credit)

    if (!is.null(caption)) {
      caption <- paste0(c(caption, paste0(prefix, credit)), collapse = collapse)
    } else {
      caption <- credit
    }
  }

  if (!is.null(caption) && !is.null(width)) {
    caption <- stringr::str_wrap(caption, width = width)
  }

  ggplot2::labs(
    ...,
    caption = caption
  )
}
