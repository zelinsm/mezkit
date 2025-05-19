#' zap_chars: Convert low-cardinality character variables to factors
#'
#' Scans a data frame for character columns and converts those with 8 or fewer unique values
#' into factors. This is useful for simplifying data cleaning and preparing variables for modeling
#' or summary tables.
#'
#' @param data A data frame or tibble.
#' @param max_levels Maximum number of unique values a character variable can have to be converted to a factor. Default is 8.
#'
#' @return A data frame where qualifying character columns have been converted to factors.
#' @examples
#' df <- tibble(
#'   sex = c("Male", "Female", "Female"),
#'   id = c("a", "b", "c"),
#'   diagnosis = c("AD", "MCI", "AD")
#' )
#' zap_chars(df)
#'
#' @export
zap_chars <- function(data, max_levels = 8) {
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(max_levels), length(max_levels) == 1, max_levels >= 1)

  dplyr::mutate(data, dplyr::across(
    .cols = dplyr::where(is.character),
    .fns = ~ if (dplyr::n_distinct(.) <= max_levels) as.factor(.) else .,
    .names = "{.col}"
  ))
}
