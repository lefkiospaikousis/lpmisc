#' Craate a 2 by n, table of means and sd's
#'
#' This function bla bla
#'
#' Notes
#' @param data A dataframe or tibble
#' @param group The group variable. Must have 2 distince values
#' @param ... The variables for the means to be calculated. DPlyr style
#'
#' @importFrom stats sd as.formula
#' @return A tibble
#'
#' @examples
#' \dontrun{
#' mtcars %>% table_means_old(group = am, disp:wt)
#' }
table_means_old <- function(data, group, ...){

  assertthat::assert_that(is.data.frame(data))

  # are columns to summarise numeric?
  purrr::map(data %>% dplyr::select(!!!rlang::enquos(...)),
             ~  assertthat::assert_that(is.numeric(.), msg = "At least one of the provided columns in ... is not numeric"))

  group <- rlang::enquo(group)

  n_grs <- data %>% dplyr::distinct(!!group) %>% dplyr::pull()

  if (length(n_grs) !=2) stop(
    paste0("Number of groups in ",
           rlang::as_name(group)," is ", length(n_grs),
           ". Must be 2. This is a 2 way comparison"),
    call. = FALSE
  )

  data %>%
    dplyr::select(!!group, ...) %>%
    tidyr::gather("key", "value", - !!group, na.rm = TRUE) %>%
    dplyr::group_by(!!group, key) %>%
    dplyr::summarise(
      mean = mean(value),
      sd   = sd(value)
    ) %>%
    tidyr::unite(mean_sd, mean, sd, sep = "_") %>%
    tidyr::spread(!!group, mean_sd) %>%
    tidyr::separate(2,
                   into = paste0(rlang::as_name(group),"_",
                                 names(.)[2],
                                 c("_mean", "_sd")),
                   sep = "_", convert = TRUE) %>%
    tidyr::separate(4,
                   into = paste0(rlang::as_name(group),"_",
                                 names(.)[4],
                                 c("_mean", "_sd")),
                   sep = "_", convert = TRUE)

}
