#' Apply Variable Labels from Codebook to Data Frame
#'
#' Uses the Hmisc::label() function to assign variable labels (e.g., survey questions) from a cleaned codebook to a raw data frame. This enhances interpretability in downstream summaries and reports.
#'
#' @param raw_df A data frame whose columns match the `variable` column in the codebook.
#' @param dd A cleaned codebook data frame with at least `variable` and `label` columns.
#'
#' @return A copy of `raw_df` with variable labels added as attributes.
#'
#' @details
#' Only variables that exist in both `raw_df` and `dd$variable`, and that have non-missing labels in `dd$label`, will be labeled.
#'
#' @examples
#' labeled_df <- labelize(raw_df, dd)
#' Hmisc::label(labeled_df$g2_disab)
#'
#' @export
labelize <- function(raw_df, dd) {
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("The 'Hmisc' package is required for labelize(). Please install it.")
  }

  labeled_df <- raw_df
  labeled_vars <- c()

  dd_labels <- dd %>%
    filter(!is.na(label), variable %in% names(raw_df))

  for (i in seq_len(nrow(dd_labels))) {
    varname <- dd_labels$variable[i]
    label_text <- dd_labels$label[i]

    Hmisc::label(labeled_df[[varname]]) <- label_text
    labeled_vars <- c(labeled_vars, varname)

    message(glue::glue("📝 Labeled '{varname}' as: \"{label_text}\""))
  }

  message(glue::glue("\n✅ Applied labels to {length(labeled_vars)} variable(s): {paste(labeled_vars, collapse = ', ')}"))

  return(labeled_df)
}
