#' Bootstrap Validation Summary for `lrm()` Models
#'
#' @description
#' Performs bootstrap internal validation on a logistic regression model
#' fitted with `rms::lrm()` and returns a tidy `tibble` summarizing key
#' performance metrics. This function extracts the apparent, optimism,
#' and validated estimates of the following:
#' - Somers' D rank correlation (`Dxy`)
#' - C-statistic (discrimination)
#' - Brier score (calibration)
#' - Nagelkerke's R-squared (explained variation)
#'
#' The function wraps the output from `rms::validate()` and transforms it
#' into a user-friendly `tibble` with interpretable columns and metric names.
#' The C-statistic is calculated explicitly as `0.5 + Dxy / 2`.
#'
#' @param val_obj An object returned by `rms::validate()` applied to an `lrm` model.
#'
#' @return A `tibble` with four rows (one per metric) and three columns:
#'   \describe{
#'     \item{metric}{Metric name: Dxy, C-statistic, Brier score, Nagelkerke R².}
#'     \item{apparent}{Performance on the training data (naively optimistic).}
#'     \item{optimism}{Average overfitting bias from bootstrap resampling.}
#'     \item{validated}{Bias-corrected estimate: apparent − optimism.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(rms)
#' data(lung, package = "survival")
#' lung <- na.omit(lung)
#'
#' dd <- datadist(lung)
#' options(datadist = "dd")
#'
#' fit <- lrm(status == 2 ~ age + sex + ph.ecog, data = lung, x = TRUE, y = TRUE)
#' val <- validate(fit, B = 200)
#'
#' lrm_val_tbl(val)
#' }
#'
#' @importFrom rms validate
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @export
lrm_val_metrics <- function(val_obj) {
  tibble::tibble(
    Metric = c("Dxy", "C-statistic", "Brier score", "Nagelkerke R²"),
    Apparent = c(
      val_obj["Dxy", "index.corrected"] + val_obj["Dxy", "optimism"],
      0.5 + (val_obj["Dxy", "index.corrected"] + val_obj["Dxy", "optimism"]) / 2,
      val_obj["B", "index.corrected"] + val_obj["B", "optimism"],
      val_obj["R2", "index.corrected"] + val_obj["R2", "optimism"]
    ),
    Optimism = c(
      val_obj["Dxy", "optimism"],
      val_obj["Dxy", "optimism"] / 2,
      val_obj["B", "optimism"],
      val_obj["R2", "optimism"]
    ),
    Validated = c(
      val_obj["Dxy", "index.corrected"],
      0.5 + val_obj["Dxy", "index.corrected"] / 2,
      val_obj["B", "index.corrected"],
      val_obj["R2", "index.corrected"]
    )
  )
}
