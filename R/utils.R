

#' Wrap a long text
#'
#' @param x String length 1
#' @param width Width to wrap it. See \code{base::strwrap}
#'
#' @return String length 1
#' @export
#'
#' @examples
#' lp_short("A very long string that needs to take more than one lines")
lp_short <- function(x, width = 30){

  strwrap(x, simplify = FALSE, width= width) %>%
    lapply(paste, collapse = "\n") %>% unlist()
}
