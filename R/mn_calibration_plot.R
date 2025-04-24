#' Calibration Plot for Multinomial Models
#'
#' Generates calibration plots for each predicted class from a multinomial model.
#' For each class, predicted probabilities are binned (default: 0.1-width bins), and the observed
#' frequency of true outcomes within each bin is plotted against the average predicted probability.
#'
#' The 45-degree dashed reference line indicates perfect calibration (i.e., predicted probabilities
#' match observed frequencies). Points above the line suggest underestimation, and points below indicate overestimation.
#'
#' @param model A fitted multinomial model (e.g., from \code{nnet::multinom()}).
#' @param data A data frame containing the original data used to fit the model.
#' @param group_name A character string used as the title for the plot (e.g., model name).
#'
#' @return A \code{ggplot2} object showing calibration curves for each predicted class.
#'
#' @examples
#' library(nnet)
#' data(iris)
#' iris_model <- multinom(Species ~ Sepal.Length + Sepal.Width, data = iris)
#' mn_calibration_plot(iris_model, iris, group_name = "Iris Model")
#'
#' @export
mn_calibration_plot <- function(model, data, group_name = "Model") {
  # Extract outcome variable name
  outcome_var <- as.character(model$call$formula[[2]])

  # Get predicted probabilities
  probs <- predict(model, newdata = data, type = "probs")

  # Handle binary case (matrix with one column)
  if (is.vector(probs)) {
    probs <- matrix(probs, ncol = 1, dimnames = list(NULL, levels(data[[outcome_var]])[2]))
  }

  # Merge probabilities with original data
  df <- cbind(data, probs)

  # Create long-format data for calibration plot
  df_long <- df |>
    dplyr::mutate(true = .data[[outcome_var]]) |>
    dplyr::select(true, dplyr::all_of(colnames(probs))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(colnames(probs)),
      names_to = "predicted_class",
      values_to = "predicted_prob"
    ) |>
    dplyr::mutate(observed = as.numeric(true == predicted_class))

  # Bin and aggregate for plotting
  df_binned <- df_long |>
    dplyr::group_by(predicted_class) |>
    dplyr::mutate(bin = cut(predicted_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) |>
    dplyr::group_by(predicted_class, bin) |>
    dplyr::summarise(
      mean_pred = mean(predicted_prob, na.rm = TRUE),
      mean_obs = mean(observed, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )

  # Build the plot
  ggplot2::ggplot(df_binned, ggplot2::aes(x = mean_pred, y = mean_obs)) +
    ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::facet_wrap(~predicted_class) +
    ggplot2::labs(
      title = paste("Calibration Plot:", group_name),
      x = "Predicted Probability (binned mean)",
      y = "Observed Proportion"
    ) +
    ggplot2::theme_minimal()
}
