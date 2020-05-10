#' Scatterplots of one variable to many others
#'
#' @param .data The dataframe
#' @param ... The variables to use. DPLYR style. Supports \code{dplyr::everything()}
#' @param target The target variable to plot scatter against all other vars
#' @param width The width of the labeller function in the strips. Defaults t0 25
#' @param .cor_method The correlation method. One of "pearson", "kendall", spearman". Defaults to "pearson"
#' @param .smouth The smooth method for the graphs. All option available in the ggplot2::geom_smouth()
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
                                 ){

  # haven;t found a good way to check the inputs here
  #
  #if(is.null(target)) stop("Hey dude! What variable is the target?")

  if(rlang::quo_is_missing(rlang::enquo(target))) stop("Hey dude! What variable is the target?")

  .data <- .data %>% dplyr::select({{target}}, ...)

  #print(.data)

  df_cor <-
    .data %>%
    inspectdf::inspect_cor(with_col = rlang::as_label(rlang::enquo(target)), method = .cor_method) %>%
    dplyr::rename("target" = col_1, "variable" = col_2)

  #print(df_cor)


  temp <-
    .data %>%
    tidyr::gather(key = "variable", value = "value", - {{target}})

  #print(temp)

  df_label <-
    temp %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(pos.y = max(value) *.9
                    , pos.x = max({{target}})*0.9) %>%
    dplyr::left_join(
        df_cor %>% dplyr::select(variable, corr)
      , by = "variable"
    )
  #print(df_label)

  temp %>%
    ggplot2::ggplot(ggplot2::aes({{target}}, value) ) +
    ggplot2::geom_point( alpha = 0.5 )+
    ggplot2::geom_smooth(method = .smouth)+
    ggplot2::geom_label(ggplot2::aes( x = pos.x
                    , y = pos.y
                    , label = round(corr, 2))
               , df_label
               , color = 'red') +
    ggplot2::facet_wrap(~variable, labeller = ggplot2::label_wrap_gen(width = width), scales = 'free_y')
}
