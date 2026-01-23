#' ds: Descriptive statistics table
#'
#' Creates a table with neat descriptive statistics.
#'
#' @param data A data frame containing numeric variables.
#' @param decimals Optional decimals to return. If `NULL` (default), 2 decimals are used.
#' @param labels Optional labels for each variable.
#' @return A tibble with columns: `var`, `N`, `Mean ± SD`, `Median ± MAD`, and `IQR`.
#' @export
ds <- function(data, decimals = 2, labels = NULL) {

  out <- data |>
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
      var = names(labels),
      label    = unname(as.character(labels))
    )

    out <- out |>
      left_join(label_df, by = "var") |>
      relocate(label, .after = var)
  }

  out
}
