#' A bar chart with percentages
#'
#' @param data The dataset
#' @param var The variable to plot
#' @param .pct_reorder Logical length 1. Reorder the bars based on value?
#' @param .drop_levels Logical length 1. Drop unused levels in the factor when counting ? i.e. do not show '0' count
#' @param .hjust Numeric length 1. The jhust for the value labels.
#' @param .title Optional. The title
#' @param .xlab The x-axis label. Defaults to the variable name
#' @param .ylab The y-axis label. Defaults to the variable name
#' @param .title Optional title
#' @param .bar_col The colour of the bars
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' lp_bar_chart(iris, Species)
lp_bar_chart <- function(data, var,

                         .pct_reorder = TRUE,
                         .drop_levels = TRUE, .hjust = -0.2,
                         .xlab = NULL, .ylab= NULL, .title = NULL, .bar_col = "#EE5C42" ){


  dta_processed <-
    data %>%
    dplyr::count({{var}}, .drop = .drop_levels) %>%
    dplyr::mutate(pct = n/sum(n),
                  bar_labels = scales::percent(pct,accuracy = 0.1)
    ) %>%
    dplyr::mutate(categories = paste0({{var}}, " (n=", n, ")"))

  if(isTRUE(.pct_reorder)){
    p <- ggplot2::ggplot(dta_processed,
                         ggplot2::aes(x = forcats::fct_reorder(categories, pct), y = pct))
  } else {

    p <- ggplot2::ggplot(dta_processed,
                         ggplot2::aes(x = forcats::fct_inorder(categories), y = pct))
  }
  p <-
    p +
    ggplot2::geom_bar(stat = "identity", width = 0.5, fill = .bar_col)+
    ggplot2::coord_flip()+
    ggplot2::geom_text(ggplot2::aes(label = bar_labels), hjust = .hjust)+
    ggplot2::scale_x_discrete( label = function(x) lp_short(x))+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1)), limits = c(0,1))+
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if(!purrr::is_null(.title)){
    p <- p + ggplot2::ggtitle(.title)
  }


  if(!purrr::is_null(.ylab)){
    p <- p + ggplot2::ylab(.ylab)
  }


  if(!purrr::is_null(.xlab)){
    p <- p + ggplot2::xlab(.xlab)
  }else {
    p <- p + ggplot2::xlab(deparse(substitute(var)))
  }



  p
}
