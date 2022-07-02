#' Faceted distribution of variables as histograms
#'
#' @param data The data
#' @param ... The variables to plot. Unquoted, dplyr way
#' @param statistic One of "mean", "median". Defaults to "mean"
#' @param .scales Character. The \code{scales} arg of ggplot2::facet_wrap(). One of c("fixed", "free","free_x", "free_y"). Defaults to "fixed"
#' @param .colour The colour of the bars. Defaults to "grey80"
#' @return A ggplot
#' @export
#'
#' @examples
#' facet_histogram(
#' mtcars, everything(), .scales = "free", statistic = "median"
#' )
#'
facet_histogram <- function(data, ..., statistic = c("mean", "median"),
                            .scales = c("fixed", "free","free_x", "free_y"),
                            .colour = "grey80"
                            ) {

  statistic <- match.arg(statistic)
  .scales <- match.arg(.scales)

  #TODO Checks
  #  valid colour
  data_long <-
    data %>%
    dplyr::select(...) %>%
    tidyr::gather("key", "value")


  data_avg <-
    data_long %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(
      mean   = mean(value, na.rm  = TRUE),
      median = median(value, na.rm = TRUE),
      sd     = sd(value, na.rm = TRUE),
      min    = min(value, na.rm = TRUE),
      max    = max(value, na.rm = TRUE)
      )

  data_long %>%
    ggplot2::ggplot(ggplot2::aes(value))+
    ggplot2::geom_histogram(colour = .colour,  binwidth = function(x) 2 * IQR(x, na.rm = TRUE) / (length(x)^(1/3)))+
    ggplot2::facet_wrap(~key, scales = .scales)+
    ggplot2::geom_vline( data = data_avg, ggplot2::aes(xintercept = !!sym(statistic)), colour = "red", linetype = "dashed")+
    ggplot2::labs(x = "",
         y = "Frequency",
         caption = paste0("--- Indicates ", statistic, " value"))+
    ggplot2::theme(strip.text = ggplot2::element_text(size = 12))

}
