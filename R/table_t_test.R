#' A table of t-tests statistics across all the variables you input
#'
#' A desctiption
#'
#' Notes
#'
#' @param data A dataframe, tibble
#' @param group The grouping variable. Must have only two distinct values
#' @param ... The variables to turn the tests across the group variable
#'
#' @return A tibble
#' @export
#' @seealso \code{\link{table_means}} for a t-test table
#'
#' @examples
#' \dontrun{
#' mtcars %>% table_means(group = am, disp:wt)}
table_t_test <- function(data, group, ...){

  group <- dplyr::enquo(group)

  temp <-
    data %>%
    select(!!group, ...) %>%
    gather(key, value, - !!group, na.rm = TRUE) %>%
    group_by(key) %>%
    nest()

  # build the formula. Note the as_name(group)
  # found it here https://community.rstudio.com/t/quasiquotation-inside-a-formula/14929/5
  lm_eq = as.formula(
    paste("value", " ~ ", rlang::as_name(group))
  )

  t_tests <-
    temp %>%
    # get t. tests in tidy format
    mutate(t_result = map(data, ~ t.test(lm_eq, data = .))) %>%
    mutate(t_tidy   = map(t_result, broom::tidy)) %>%
    unnest(t_tidy, .drop = TRUE)

  return(list(
    temp,
    t_tests
  ))
}
