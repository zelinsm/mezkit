#' Pretty Confusion Matrix (Transposed: Predicted in Rows)
#'
#' Create a nicely formatted confusion matrix where Predicted classes are in rows and Observed classes are in columns.
#'
#' @param predicted The predicted labels.
#' @param actual The actual observed labels.
#' @param group_name A label for the model or group (appears in table title).
#'
#' @return A confusion matrix table with Predicted classes as rows.
#' @export
prty_confmat2 <- function(predicted, actual, group_name = "Model") {
  # Create confusion matrix: Predicted rows, Observed columns
  conf_mat <- table(Predicted = predicted, Observed = actual)

  # Turn into a data frame
  conf_df <- as.data.frame.matrix(conf_mat)

  # Add total predicted (row sums)
  conf_df$Total_Predicted <- rowSums(conf_df)

  # Get column totals
  col_totals <- colSums(conf_mat)
  grand_total <- sum(conf_mat)

  # Make a totals row
  totals_row <- c(as.list(col_totals), Total_Predicted = grand_total)

  # Combine
  conf_complete <- conf_df %>%
    tibble::rownames_to_column("Predicted") %>%
    bind_rows(c(Predicted = "Total_Observed", totals_row))

  # Create the gt table
  conf_complete %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Confusion Matrix (Predicted vs Observed): ", group_name, "**"))
    ) %>%
    tab_spanner(
      label = "Observed Class",
      columns = names(conf_df)[names(conf_df) != "Total_Predicted"]
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      table.font.size = "small",
      column_labels.font.weight = "bold"
    )
}
