#' factorability: Quick report for KMO and Bartlett's test results
#'
#' This function computes and prints a quick summary of the Kaiser-Meyer-Olkin (KMO)
#' Measure of Sampling Adequacy and Bartlett’s Test of Sphericity to assess whether
#' data are appropriate for exploratory factor analysis (EFA).
#'
#' @param data A data frame or tibble with numeric variables to assess factorability.
#' @param digits Number of decimal places to round KMO and chi-squared values. Default is 2.
#'
#' @return No return value. Prints a concise report to the console with:
#' - Overall KMO value
#' - Bartlett’s test chi-squared value, degrees of freedom, and formatted p-value
#'
#' @examples
#' \dontrun{
#' factorability(mtcars[, 1:5])
#' }
#'
#' @export
factorability <- function(data, digits = 2) {
  stopifnot(is.data.frame(data))

  kmo_result <- psych::KMO(data)
  bartlett_result <- psych::cortest.bartlett(cor(data, use = "pairwise.complete.obs"), n = nrow(data))

  kmo <- round(kmo_result$MSA, digits)
  chi2 <- round(bartlett_result$chisq, digits)
  df <- bartlett_result$df
  p <- bartlett_result$p.value

  p_formatted <- if (p < .001) {
    "p < .001"
  } else {
    paste0("p = ", sprintf(paste0("%.", digits, "f"), p))
  }

  cat("KMO =", kmo, "; χ²(", df, ") =", chi2, ",", p_formatted, "\n")
}
