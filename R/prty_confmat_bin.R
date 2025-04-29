#' Pretty Confusion Matrix (Transposed: Predicted in Rows)
#'
#' Create a nicely formatted confusion matrix for a model with a binary outcome where Predicted classes are in rows and Observed classes are in columns.
#'
#' @param predicted The predicted labels.
#' @param actual The actual observed labels.
#' @param group_name A label for the model or group (appears in table title).
#'
#' @return A confusion matrix table with Predicted classes as rows.
#' @export
prty_confmat_bin <- function(
    model,
    data,
    outcome_var,
    positive_class = 1,
    negative_class = 0,
    method = maximize_metric,
    metric = sum_sens_spec,
    decimals = 3,
    title = NULL,
    subtitle = NULL,
    positive_label = "Yes",
    negative_label = "No",
    threshold = NULL,
    return_type = c("table", "list")
) {
  library(dplyr)
  library(gt)
  library(broom)
  library(cutpointr)
  library(caret)
  library(tibble)
  library(glue)
  library(rlang)

  return_type <- match.arg(return_type)

  outcome_var <- ensym(outcome_var)
  outcome_var_str <- as_string(outcome_var)

  # Check that the outcome variable is binary
  outcome_vals <- unique(na.omit(data[[outcome_var_str]]))
  if (length(outcome_vals) != 2) {
    stop("This function currently supports binary classification only. Please use mezkit::prty_confmat2 for multiclass support.")
  }

  # Get model predictions
  aug <- broom::augment(model, type.predict = "response", newdata = data)

  # Ensure predictions and outcome have no NAs
  aug <- aug %>%
    filter(!is.na(.fitted), !is.na(!!outcome_var))

  if (!".fitted" %in% names(aug)) {
    stop("Predicted probabilities (.fitted) not found. Ensure model supports augment(type.predict = 'response').")
  }

  # Determine threshold
  if (is.null(threshold)) {
    cp <- cutpointr(
      data = aug,
      x = .fitted,
      class = !!outcome_var,
      pos_class = positive_class,
      neg_class = negative_class,
      method = method,
      metric = metric
    )
    best_cut <- cp$optimal_cutpoint
    cutpoint_method <- deparse(substitute(metric))
  } else {
    best_cut <- threshold
    cutpoint_method <- glue("user-defined threshold = {threshold}")
  }

  # Create predicted and true binary vectors
  pred_binary <- factor(aug$.fitted >= best_cut, levels = c(FALSE, TRUE))
  true_binary <- factor(aug[[outcome_var_str]] == positive_class, levels = c(FALSE, TRUE))

  cm <- caret::confusionMatrix(pred_binary, true_binary, positive = "TRUE")

  # Convert confusion matrix to tidy format
  cm_table <- as.table(cm$table)
  conf_df <- as_tibble(as.data.frame.matrix(cm_table), rownames = "Prediction") |>
    mutate(
      Total_Predicted = `FALSE` + `TRUE`
    ) |>
    rename(
      !!glue("Observed: {negative_label}") := `FALSE`,
      !!glue("Observed: {positive_label}") := `TRUE`
    ) |>
    mutate(Prediction = dplyr::recode(Prediction, "FALSE" = negative_label, "TRUE" = positive_label)) |>
    bind_rows(
      tibble(
        Prediction = "Total Observed",
        !!glue("Observed: {negative_label}") := sum(cm_table[, "FALSE"]),
        !!glue("Observed: {positive_label}") := sum(cm_table[, "TRUE"]),
        Total_Predicted = sum(cm_table)
      )
    )

  # Format gt table
  gt_table <- conf_df |>
    gt() |>
    tab_header(
      title = title %||% glue("Confusion Matrix (Predicted vs Observed): {deparse(substitute(model))}"),
      subtitle = subtitle %||% glue(
        "Cutpoint: Pr({outcome_var_str} = {positive_class}) >= {round(best_cut, 4)} ",
        "({cutpoint_method})"
      )
    ) |>
    cols_label(
      Prediction = "Prediction",
      Total_Predicted = "Total Predicted"
    ) |>
    fmt_number(columns = where(is.numeric), decimals = 0) |>
    opt_align_table_header("center") |>
    tab_source_note(
      source_note = glue(
        "Accuracy = {round(cm$overall['Accuracy'], decimals)}; ",
        "Balanced Accuracy = {round(cm$byClass['Balanced Accuracy'], decimals)}; ",
        "Specificity = {round(cm$byClass['Specificity'], decimals)}; ",
        "Sensitivity = {round(cm$byClass['Sensitivity'], decimals)}"
      )
    )

  if (return_type == "list") {
    return(list(
      table = gt_table,
      metrics = cm,
      cutpoint = best_cut
    ))
  } else {
    return(gt_table)
  }
}
