#' ds: Descriptive statistics table
#'
#' Creates a tidy table of descriptive statistics for all numeric variables
#' in a data frame. Non-numeric variables are automatically ignored.
#'
#' @param data A data frame. All numeric variables will be summarized.
#' @param decimals Number of decimal places to round summary statistics.
#'   Defaults to 2.
#' @param labels Optional named character vector mapping variable names
#'   to human-readable labels.
#'
#' @return A tibble with one row per variable and columns:
#'   `var`, `N`, `Mean ± SD`, `Median ± MAD`, and `IQR`.
#'
#' @examples
#' df <- data.frame(
#'   age = c(34, 29, 42, NA),
#'   score = c(88.2, 91.5, 79.3, 85.0),
#'   group = c("A", "A", "B", "B")
#' )
#'
#' ds(df)
#'
#' ds(df, decimals = 1)
#'
#' ds(
#'   df,
#'   labels = c(
#'     age = "Participant age",
#'     score = "Test score"
#'   )
#' )
#'
#' @export
ds <- function(data, decimals = 2, labels = NULL) {

  num_vars <- dplyr::select(data, where(is.numeric))
  if (ncol(num_vars) == 0) {
    stop("ds(): no numeric variables detected in `data`")
  }

  out <- num_vars |>
    summarise(
      across(
        everything(),
        list(
          N      = ~ sum(!is.na(.x)),
          mean   = ~ mean(.x, na.rm = TRUE),
          sd     = ~ sd(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          mad    = ~ mad(.x, na.rm = TRUE),
          iqr    = ~ IQR(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = c("var", "stat"),
      names_sep = "__"
    ) |>
    pivot_wider(
      names_from = stat,
      values_from = value
    ) |>
    mutate(
      across(
        c(mean, sd, median, mad, iqr),
        ~ round(.x, decimals)
      ),
      `Mean ± SD`    = paste0(mean, " ± ", sd),
      `Median ± MAD` = paste0(median, " ± ", mad),
      IQR            = iqr,
      N              = as.integer(N)
    ) |>
    select(
      var,
      N,
      `Mean ± SD`,
      `Median ± MAD`,
      IQR
    )

  if (!is.null(labels)) {
    label_df <- tibble(
      var   = names(labels),
      label = unname(as.character(labels))
    )

    out <- out |>
      left_join(label_df, by = "var") |>
      relocate(label, .after = var)
  }

  out
}
