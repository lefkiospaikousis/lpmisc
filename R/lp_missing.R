#' Count missing observations
#'
#' This function counts the number of missing observations per variable
#'
#' @param .data A dataframe, or an aboject that can be coerced to dataframe
#' @param ... Unused yet
#'
#' @return A tibble
#' @export
#'
#' @examples
#' lp_missing(airquality)
lp_missing <- function(.data, ...){

  .data %>%
    purrr::map_int(~ sum(is.na(.))) %>%
    tibble::enframe("variable", "n_missing")
}
