#' Pretty Confusion Matrix (Predicted in Columns)
#'
#' Create a nicely formatted confusion matrix where Predicted classes are in columns and Observed classes are in rows.
#'
#' @param actual The actual observed labels.
#' @param predicted The predicted labels.
#'
#' @return A confusion matrix table.
#' @export
prty_confmat <- function(predicted, actual, group_name = "Model") {
  # Create confusion matrix: Observed rows, Predicted columns
  conf_mat <- table(Observed = actual, Predicted = predicted)

  # Turn into a data frame
  conf_df <- as.data.frame.matrix(conf_mat)

  # Add total observed (row sums)
  conf_df$Total_Observed <- rowSums(conf_df)

  # Get column totals
  col_totals <- colSums(conf_mat)
  grand_total <- sum(conf_mat)

  # Make a totals row
  totals_row <- c(as.list(col_totals), Total_Observed = grand_total)

  # Combine
  conf_complete <- conf_df %>%
    tibble::rownames_to_column("Observed") %>%
    bind_rows(c(Observed = "Total_Predicted", totals_row))

  # Create the gt table
  conf_complete %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Confusion Matrix (Observed vs Predicted): ", group_name, "**"))
    ) %>%
    tab_spanner(
      label = "Predicted Class",
      columns = names(conf_df)[names(conf_df) != "Total_Observed"]
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
