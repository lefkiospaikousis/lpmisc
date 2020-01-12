#' Calculate the Test- Retest correlation among the variables
#'
#' This function takes two dataframes with the same col names and an id variable and calculates
#' the correlation within each column as a test-retest reliability index.
#'
#' Checks whether the column names are the same, and if there is an `id` variable for the subjects
#'
#' @param .test The TEST dataframe
#' @param .retest The RE-TEST datafra,e
#' @param .id The id variable for the subjects (defaults to `id`)
#' @param .method The correlation method. "pearson","spearman", "kendall"? see \code{?corrr::correlate}
#'
#' @return A tibble of the correaltion coefficient per variable
#' @export
#'
#' @examples
lp_testRtest <- function(.test, .retest, .id = "id", .method = "pearson"){


  if(!purrr::is_empty(setdiff(names(.test), names(.retest)))){
    stop("Dataframes do not have the same column names")
  }

  if(!.id %in% names(.test)){
    stop("Have you set the `id` variable? We need an `id` variable in the dataframe")
  }

  df_names <- dplyr::setdiff(names(.test), .id)

  .test %>%
    dplyr::left_join(.retest, by = .id, suffix = c("test", "retest")) %>%
    dplyr::select(-.id) %>%
    corrr::correlate(method = .method) %>%
    # What we need is the correaltions bewtween retest and test
    # here we have between and among each other
    dplyr::select(rowname, dplyr::ends_with("retest")) %>%
    dplyr::filter(stringr::str_detect(rowname, "\\dtest$")) %>%
    # remove the rowname variable to get a matrix
    dplyr::select(-rowname) %>%
    as.matrix() %>%
    # we are interested in the diagonal only
    diag() %>%
    # giv name to get a nice dataframe
    purrr::set_names(nm = df_names) %>%
    tibble::enframe("var", "r")
}


lp_testRtest(test_part1, retest_part1)
