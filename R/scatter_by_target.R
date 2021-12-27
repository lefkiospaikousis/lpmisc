#' Scatterplots of one variable vs many others
#'
#' This function plots a multi-panel scatterplot where many variables are plotted against
#' a target variable
#'
#' @param .data The dataframe
#' @param ... The variables to use. DPLYR style. Supports \code{dplyr::everything()}
#' @param target The target variable to plot scatter against all other vars
#' @param width The width of the labeller function in the strips. Defaults t0 25
#' @param .cor_method The correlation method. One of "pearson", "kendall", spearman". Defaults to "pearson"
#' @param .smouth The smooth method for the graphs. All option available in the ggplot2::geom_smouth()
#' @param .scales Character. The \code{scales} arg of ggplot2::facet_wrap(). One of c("fixed", "free","free_x", "free_y"). Defaults to "fixed"
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' lp_scatter_by_target(mtcars, target = mpg, disp:wt)
lp_scatter_by_target <- function(.data, target = NULL, ...
                                 ,  width = 25
                                 , .cor_method = "pearson"
                                 , .smouth = "lm"
                                 , .scales = c("free_y", "free", "fixed","free_x")
){

  # haven;t found a good way to check the inputs here
  #
  #if(is.null(target)) stop("Hey dude! What variable is the target?")

  .scales <- match.arg(.scales)

  if(rlang::quo_is_missing(rlang::enquo(target))) stop("Hey dude! What variable is the target?")

  .data <- .data %>% dplyr::select({{target}}, ...)

  dta_long <-
    .data %>%
    tidyr::gather(key = "variable", value = "value", - {{target}})

  df_cor <-
    dta_long %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      corr = cor({{target}}, value, method = .cor_method),
      pos.y = max(value) *.9,
      pos.x = max({{target}})*0.9
    )

  dta_long %>%
    ggplot2::ggplot(ggplot2::aes({{target}}, value) ) +
    ggplot2::geom_point( alpha = 0.5 )+
    ggplot2::geom_smooth(method = .smouth)+
    ggplot2::geom_label(ggplot2::aes( x = pos.x
                                      , y = pos.y
                                      , label = paste0("r = ", round(corr, 2)))
                        , df_cor
                        , color = 'red') +
    ggplot2::facet_wrap(~variable, labeller = ggplot2::label_wrap_gen(width = width), scales = .scales)
}
