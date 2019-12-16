#' Craate a table of means and sd's
#'
#' This function bla bla
#'
#' Notes
#' @param .data A dataframe or tibble
#' @param group The group variable. Must be of type character or factor
#' @param ... The variables for the means to be calculated. DPlyr style
#' @param add_totals Logical. Add the overall aggregations? Defaults to FALSE.
#'
#' @importFrom stats sd as.formula
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars %>% table_means(group = am, disp:wt)
#' }
table_means <- function(.data, group, ..., add_totals = FALSE){

  assertthat::assert_that(is.data.frame(.data))

  # group_var = rlang::as_name(quote(group))
  #
  # if(!is.factor(.data$group_var)) stop(paste0(group_var, " is not factor"))

  if(dplyr::is.grouped_df(.data)) {
    .data = dplyr::ungroup(.data)
    message("Ignoring previous groupings via the group_by")
    }
#
  # are columns to summarise numeric?
  purrr::map(.data %>% dplyr::select(!!!rlang::enquos(...)),
             ~  assertthat::assert_that(is.numeric(.), msg = "At least one of the provided columns in ... is not numeric"))

  # somthing here causes error. Activate and run to see
  # group_text = rlang::as_label(rlang::quo(group))
  #
  # assertthat::assert_that(
  #   any(is.character(.data[group_text]), is.factor(.data[group_text]))
  # )

  group <- rlang::enquo(group)

  n_grs <- .data %>% dplyr::distinct(!!group) %>% dplyr::pull()


  Totals =
    .data %>%
    dplyr::select(...) %>%
    tidyr::gather("variable", "value", na.rm = TRUE) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      mean = mean(value),
      sd   = sd(value)
    ) %>%
    dplyr::mutate(Total = paste0( round(mean, 1)," (" , round(sd, 1), ")")) %>%
    dplyr::select(-mean, -sd)


  .data %>%
    dplyr::select(!!group, ...) %>%
    tidyr::gather("variable", value, - !!group, na.rm = TRUE) %>%
    dplyr::group_by(!!group, variable) %>%
    dplyr::summarise(
      mean = mean(value),
      sd = sd(value)
    ) %>%
    dplyr::mutate(mean_sd = paste0( round(mean, 1)," (" , round(sd, 1), ")")) %>%
    dplyr::select(-mean, -sd) %>%
    tidyr::spread(!!group, mean_sd) %>%
    {if(add_totals) dplyr::left_join(., Totals, by = "variable") else .}

}
