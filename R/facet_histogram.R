#' Faceted distribution of variables as histograms
#'
#' @param data The data
#' @param ... The variables to plot. Unquoted, dplyr way
#' @param statistic One of "mean", "median". Defaults to "mean"
#'
#' @return A ggplot
#' @export
#'
#' @examples
facet_histogram <- function(data, ..., statistic = c("mean", "median")) {

  statistic <- match.arg(statistic)

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
    ggplot(aes(value))+
    geom_histogram(colour = "grey80", binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
    facet_wrap(~key, scales = "free_x")+
    geom_vline( data = data_avg, aes(xintercept = !!sym(statistic)), colour = "red", linetype = "dashed")+
    labs(x = "",
         y = "Frequency",
         caption = paste0("--- Indicates ", statistic, " value"))+
    theme(strip.text = element_text(size = 12))+
    ggthemes::theme_clean()

}
