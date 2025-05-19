#' ezcor: Easily view high correlations in tidy format
#'
#' Computes pairwise correlations for numeric variables in a data frame and returns
#' a tidy table of unique variable pairs. Optionally filters for absolute correlation
#' above a user-specified threshold. Automatically selects only numeric columns and
#' excludes pairs with missing correlation values.
#'
#' @param data A data frame containing variables to check for multicollinearity.
#' @param thresh Optional numeric threshold (between 0 and 1). Only variable pairs with absolute correlation greater than this value will be shown. If `NULL` (default), all unique correlations are returned.
#'
#' @return A tibble with columns: `var1`, `var2`, and `correlation`, sorted by descending absolute correlation.
#' @examples
#' ezcor(mtcars)               # View all unique correlations
#' ezcor(mtcars, thresh = 0.8) # View only high correlations
#'
#' @export
ezcor <- function(data, thresh = NULL) {
  stopifnot(is.data.frame(data))
  if (!is.null(thresh)) {
    stopifnot(is.numeric(thresh), length(thresh) == 1, thresh >= 0, thresh <= 1)
  }

  numeric_data <- data |> dplyr::select(dplyr::where(is.numeric))

  cor_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")
  tidy_cor <- cor_matrix |>
    tibble::as_tibble(rownames = "var1") |>
    tidyr::pivot_longer(-var1, names_to = "var2", values_to = "correlation") |>
    dplyr::filter(var1 < var2, !is.na(correlation))

  if (!is.null(thresh)) {
    tidy_cor <- dplyr::filter(tidy_cor, abs(correlation) > thresh)
  }

  dplyr::arrange(tidy_cor, dplyr::desc(abs(correlation)))
}

