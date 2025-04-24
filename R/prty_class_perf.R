#' Pretty Class Performance Table
#'
#' Generates a table of per-class classification metrics: Sensitivity, Specificity, Positive Predictive Value (PPV),
#' Negative Predictive Value (NPV), and Balanced Accuracy. Works for any classification task with two or more classes.
#'
#' Predicted and actual labels are passed directly as vectors. The function supports automatic color highlighting
#' of best/worst metrics per column, as well as optional bolding.
#'
#' @param predicted A vector of predicted class labels.
#' @param actual A vector of observed (true) class labels.
#' @param model_name A character label used in the table title.
#' @param color Logical. If TRUE, adds color highlighting to metric cells.
#' @param highlight Character. One of "best", "worst", "both". Controls which values to highlight with color.
#'
#' @return A \code{gt} table with per-class metrics and overall accuracy in the subtitle.
#'
#' @examples
#' library(nnet)
#' model <- multinom(Species ~ Sepal.Length + Sepal.Width, data = iris)
#' predicted <- predict(model)
#' prty_class_stats(predicted, iris$Species, model_name = "Iris Model", color = TRUE, highlight = both)
#'
#' @export

prty_class_perf <- function(predicted, actual, model_name = "Model", color = FALSE, highlight = "best") {

  # Create confusion matrix: Observed rows, Predicted columns
  conf_mat <- table(Observed = actual, Predicted = predicted)

  # Overall Accuracy
  correct_predictions <- sum(diag(conf_mat))
  total_predictions <- sum(conf_mat)
  overall_accuracy <- round(100 * correct_predictions / total_predictions, 2)

  # Metrics by class
  metrics <- lapply(1:nrow(conf_mat), function(i) {
    TP <- conf_mat[i, i]
    FN <- sum(conf_mat[i, ]) - TP
    FP <- sum(conf_mat[, i]) - TP
    TN <- total_predictions - TP - FN - FP

    sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
    specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
    ppv <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
    npv <- ifelse((TN + FN) > 0, TN / (TN + FN), NA)
    balanced_accuracy <- ifelse(!is.na(sensitivity) & !is.na(specificity),
                                (sensitivity + specificity) / 2,
                                NA)

    tibble(
      Class = rownames(conf_mat)[i],
      Sensitivity = 100 * sensitivity,
      Specificity = 100 * specificity,
      PPV = 100 * ppv,
      NPV = 100 * npv,
      Balanced_Accuracy = 100 * balanced_accuracy
    )
  }) |> bind_rows() |>
    mutate(across(where(is.numeric), ~ round(.x, 2)))

  # Find target values for highlighting
  max_values <- metrics |>
    select(Sensitivity, Specificity, PPV, NPV, Balanced_Accuracy) |>
    summarise(across(everything(), max, na.rm = TRUE))

  min_values <- metrics |>
    select(Sensitivity, Specificity, PPV, NPV, Balanced_Accuracy) |>
    summarise(across(everything(), min, na.rm = TRUE))

  # Build initial gt table
  gt_table <- metrics |>
    gt() |>
    tab_header(
      title = md(paste0("**Per-Class Classification Metrics: ", model_name, "**")),
      subtitle = md(paste0("**Overall Accuracy: ", overall_accuracy, "%**"))
    ) |>
    tab_spanner(
      label = "Performance Metrics (%)",
      columns = c(Sensitivity, Specificity, PPV, NPV, Balanced_Accuracy)
    ) |>
    fmt_number(
      columns = where(is.numeric),
      decimals = 2
    ) |>
    cols_align(
      align = "center",
      columns = everything()
    ) |>
    tab_options(
      table.font.size = "small",
      column_labels.font.weight = "bold",
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(14)
    )

  # Apply color and bolding if requested
  if (color) {

    if (highlight == "best" || highlight == "worst") {

      # Simple two-color function
      simple_color_fn <- function(x) {
        if (highlight == "best") {
          ifelse(x == max(x, na.rm = TRUE), "deepskyblue", "white")
        } else {
          ifelse(x == min(x, na.rm = TRUE), "lightpink", "white")
        }
      }

      gt_table <- gt_table |>
        data_color(
          columns = c(Sensitivity, Specificity, PPV, NPV, Balanced_Accuracy),
          fn = simple_color_fn
        )

    } else if (highlight == "both") {

      # More complex: both max (blue) and min (pink)
      both_color_fn <- function(x) {
        ifelse(
          x == max(x, na.rm = TRUE), "deepskyblue",
          ifelse(x == min(x, na.rm = TRUE), "lightpink", "white")
        )
      }

      gt_table <- gt_table |>
        data_color(
          columns = c(Sensitivity, Specificity, PPV, NPV, Balanced_Accuracy),
          fn = both_color_fn
        )
    }

    # Always bold the best values only
    for (col in c("Sensitivity", "Specificity", "PPV", "NPV", "Balanced_Accuracy")) {
      gt_table <- gt_table |>
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(
            columns = all_of(col),
            rows = metrics[[col]] == max_values[[col]]
          )
        )
    }
  }

  gt_table
}
