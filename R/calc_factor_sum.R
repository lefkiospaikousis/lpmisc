#' Calculates factor scores by summing the variables
#'
#' This function takes a dataframe, and a series of variables and calculates the
#' rowwise sum. It should be usefull to calculate factor scores out of several variables
#'
#' Beware that
#' * The output is a vector of numeric values. To use this in a piping syntax
#' see the example below - note the use of '.' in the pipe worlflow
#' * The sum throws out the missing values (na.rm = TRUE) so you will always get a sum despite the missing values in you variables
#'
#' @param .data A dataframe
#' @param ... Variables to sum. Select variables DPLYR style.
#' One or more unquoted expressions separated by commas.
#' You can treat variable names like they are positions, so you can use expressions like x:y to select ranges of variables
#' You can also pass variables as vector of strings
#'
#' @return A vector of numeric values representing the factor scores per subject
#' @export
#'
#' @examples
#' mtcars %>% dplyr::mutate(score = calc_factor_sum(., mpg, cyl)) %>% head()
calc_factor_sum = function (.data, ...){

  # TO DO: Need some checks here
  # a. is.dataframe
  # b. Numeric variables in ...

  .data %>%
    dplyr::select(...) %>%
    dplyr::transmute(temp = purrr::pmap_dbl( ., sum, na.rm=T)) %>%
    dplyr::pull()

}
