#' ezkmo: Display KMO values by variable
#'
#' This function calculates and displays the Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy
#' for each variable in a data frame, using the `KMO()` function from the `psych` package.
#' Optionally, it adds interpretation categories and returns a formatted `gt` table.
#'
#' Interpretation thresholds:
#' - ≥ 0.90 = Marvelous
#' - ≥ 0.80 = Meritorious
#' - ≥ 0.70 = Middling
#' - ≥ 0.60 = Mediocre
#' - ≥ 0.50 = Miserable
#' - <  0.50 = Unacceptable
#'
#' @param data A data frame or tibble containing numeric variables for KMO assessment.
#' @param interpret Logical. If TRUE, adds interpretation labels based on standard KMO thresholds. Default is FALSE.
#' @param gt Logical. If TRUE, returns a formatted `gt` table. If FALSE, returns a tibble. Default is FALSE.
#'
#' @return A tibble or `gt` table summarizing KMO values per variable.
#' @examples
#' \dontrun{
#' ezkmo(mtcars)
#' ezkmo(mtcars, interpret = TRUE)
#' ezkmo(mtcars, interpret = TRUE, gt = TRUE)
#' }
#' @export
ezkmo <- function(data, interpret = FALSE, gt = FALSE) {
  stopifnot(is.data.frame(data))

  # Calculate KMO using psych package
  kmo_result <- psych::KMO(data)
  kmo_table <- tibble::tibble(
    Variable = names(kmo_result$MSAi),
    KMO_Value = as.numeric(kmo_result$MSAi)
  )

  if (interpret) {
    kmo_table <- dplyr::mutate(kmo_table,
                               Interpretation = dplyr::case_when(
                                 KMO_Value >= 0.90 ~ "Marvelous",
                                 KMO_Value >= 0.80 ~ "Meritorious",
                                 KMO_Value >= 0.70 ~ "Middling",
                                 KMO_Value >= 0.60 ~ "Mediocre",
                                 KMO_Value >= 0.50 ~ "Miserable",
                                 TRUE              ~ "Unacceptable"
                               )
    )
  }

  kmo_table <- dplyr::arrange(kmo_table, KMO_Value)

  if (gt) {
    gt::gt(kmo_table) |>
      gt::fmt_number(columns = "KMO_Value", decimals = 3) |>
      gt::tab_header(title = "KMO Measure of Sampling Adequacy per Variable")
  } else {
    kmo_table
  }
}
