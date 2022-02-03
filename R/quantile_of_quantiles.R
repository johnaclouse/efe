#' quantil_of_quantiles
#'
#' Calculate quantiles for each input, sum the input quantiles, and calculate
#' quantile of the sum.
#'
#' Note: NA values in any of the input vectors will result in a NA value for the
#' quantile of quantiles.
#'
#' @param df data frame
#' @param inputs character vector with the names of the columns to be converted
#'   to a quantile of quantiles
#'
#' @return numeric vector of quantiles
#' @export
#'
#' @examples
quantile_of_quantiles <- function(df, inputs = NULL){

  . <- input_sum <- NULL # avoid R CMD Check error for undefined global variable

  # df = eeda::eeda_test_data
  inputs = c("key_recency", "key_frequency", "key_monetary")

  df %>%
    dplyr::select(tidyselect::any_of(inputs)) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(inputs),
        dplyr::percent_rank
      )
    ) %>%
    dplyr::mutate(input_sum = rowSums(.),
                  combined_quantile = dplyr::percent_rank(input_sum)
    )
}

