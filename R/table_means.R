#' Craate a 2 by n, table of means and sd's
#'
#' This function bla bla
#'
#' Notes
#' @param data A dataframe, tibble
#' @param group The group variable. Must have 2 distince values
#' @param ... The variables for the means to be calculated. DPlyr style
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars %>% table_means(group = am, disp:wt)
#' }
table_means <- function(data, group, ...){

  group <- dplyr::enquo(group)

  n_grs <- data %>% dplyr::distinct(!!group) %>% dplyr::pull()

  #assertthat::assert_that(length(n_grs)==2)

  if (length(n_grs) !=2) stop(
    paste0("Number of groups is ", length(n_grs), ". Must be 2. This is a t.test remember?")
  )

  data %>%
    select(!!group, ...) %>%
    gather(key, value, - !!group, na.rm = TRUE) %>%
    group_by(!!group, key) %>%
    summarise(
      mean = mean(value),
      sd   = sd(value)
    ) %>%
    unite(mean_sd, mean, sd, sep = "_") %>%
    spread(!!group, mean_sd) %>%
    separate(2,
             into = paste0(names(.)[2],
                           c("_mean", "_sd")),
             sep = "_", convert = TRUE) %>%
    separate(4,
             into = paste0(names(.)[4],
                           c("_mean", "_sd")),
             sep = "_", convert = TRUE)

}
