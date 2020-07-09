#' Summarises many variables
#'
#' This function cretes a summary table of multiple variables. It is unsing tidyevaluation
#' to gather the variables provided, and provide pre-defined summary statistics.
#' You can also supply grouped tibbles, to get summary statistics per group
#'
#' @param .data A tibble
#' @param ... Select variables DPLYR style.
#' One or more unquoted expressions separated by commas. You can treat variable names like they are positions, so you can use expressions like x:y to select ranges of variables
#'
#' @return A tibble with summaries
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars %>%
#' group_by(vs) %>%
#' lp_summarise(mpg, cyl)
#' }
lp_summarise <- function(.data, ...){


    # user deifned functions
  se = function(sd, n, na.rm = TRUE){
    sd/sqrt(n)
  }

  missing = function(x) sum(is.na(x))

  valid_n = function(x) sum(!is.na(x))

  # list of functions. includes the user defined se
  my_summary = list(
    ~mean(., na.rm = TRUE),
    ~sd(., na.rm = TRUE),
    ~se(sd(., na.rm = TRUE),sum(!is.na(.))),
    ~min(., na.rm = TRUE),
    ~max(., na.rm = TRUE),
    ~missing(.),
    ~valid_n(.)
  )


  .data %>%
    dplyr::select_at(dplyr::vars(...)) %>%
    tidyr::gather("key", "value", - dplyr::one_of(dplyr::group_vars(.))) %>%
    dplyr::group_by(key, .add = TRUE) %>%
    dplyr::summarise_at(dplyr::vars(value), my_summary)
}

