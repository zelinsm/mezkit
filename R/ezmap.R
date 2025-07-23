#' Recode Variables in a Data Frame Using Codebook Mapping
#'
#' Safely recodes variables in a raw dataset based on the `levels_mapped` column of a cleaned codebook (e.g., from REDCap). This function evaluates the character strings in `levels_mapped`, converts them to named character vectors, and uses them to map raw values to labeled factors. It ensures mappings are safe by checking for complete coverage and preventing overwriting data with `NA`s.
#'
#' @param raw_df A data frame containing raw data to be recoded. Column names must match the `variable` names in the codebook.
#' @param dd A cleaned codebook data frame containing at least the columns `variable` and `levels_mapped`. `levels_mapped` must contain valid R character strings like `"1" = "Yes", "0" = "No"`, which can be evaluated into named vectors.
#'
#' @return A data frame with the same structure as `raw_df`, but with applicable variables converted to labeled factors.
#'
#' @details
#' - Only rows in the codebook where `levels_mapped` is non-missing and `variable` is present in `raw_df` are considered.
#' - Variables are skipped if:
#'   - The `levels_mapped` string cannot be parsed
#'   - The mapping does not cover all non-missing values in the raw column
#'   - The recoding would result in complete loss of information (all values become `NA`)
#'
#' @section Logging:
#' For each variable:
#' - ✅ A message is printed for each successful mapping
#' - ⚠️ A warning is issued for skipped mappings due to incomplete coverage
#' - ❌ A warning is issued if a mapping is malformed or causes data loss
#' - A summary of all mapped, skipped, and failed variables is printed at the end
#'
#' @examples
#' # Assume dd is a cleaned codebook created by zap_dd()
#' # Assume raw_df contains columns listed in dd$variable
#' mapped_df <- ezmap(raw_df, dd)
#'
#' @name ezmap
#' @export


library(dplyr)
library(rlang)
library(stringr)
library(purrr)
library(glue)

ezmap <- function(raw_df, dd) {
  recoded_df <- raw_df
  mapped_vars <- c()
  skipped_vars <- c()
  failed_vars <- c()

  dd_to_map <- dd %>%
    filter(!is.na(levels_mapped), variable %in% names(raw_df))

  for (i in seq_len(nrow(dd_to_map))) {
    varname <- dd_to_map$variable[i]
    mapping_string <- dd_to_map$levels_mapped[i]
    raw_col <- raw_df[[varname]]

    expr_text <- paste0("c(", mapping_string, ")")
    parsed <- tryCatch(eval(parse(text = expr_text)), error = function(e) NULL)

    if (is.null(parsed)) {
      warning(glue("❌ Skipping '{varname}': could not parse levels_mapped."))
      failed_vars <- c(failed_vars, varname)
      next
    }

    # Check: does the mapping cover all values in the column?
    unique_vals <- unique(na.omit(as.character(raw_col)))
    unmapped_vals <- setdiff(unique_vals, names(parsed))

    if (length(unmapped_vals) > 0) {
      warning(glue("⚠️ Skipping '{varname}': mapping does not cover all values (unmapped: {paste(unmapped_vals, collapse=', ')})."))
      skipped_vars <- c(skipped_vars, varname)
      next
    }

    # Attempt recode and check for data loss
    recoded_col <- factor(as.character(raw_col), levels = names(parsed), labels = as.character(parsed))

    if (all(is.na(recoded_col)) && any(!is.na(raw_col))) {
      warning(glue("⚠️ Skipping '{varname}': mapping resulted in complete data loss (all NAs)."))
      failed_vars <- c(failed_vars, varname)
      next
    }

    recoded_df[[varname]] <- recoded_col
    message(glue("✅ Mapped '{varname}' with {length(parsed)} levels"))
    mapped_vars <- c(mapped_vars, varname)
  }

  message(glue("\n✔️  Successfully mapped {length(mapped_vars)} variable(s): {paste(mapped_vars, collapse = ', ')}"))

  if (length(skipped_vars) > 0) {
    message(glue("⚠️  Skipped {length(skipped_vars)} variable(s) due to incomplete coverage: {paste(skipped_vars, collapse = ', ')}"))
  }

  if (length(failed_vars) > 0) {
    message(glue("❌  Skipped {length(failed_vars)} variable(s) due to malformed mappings or full NA conversion: {paste(failed_vars, collapse = ', ')}"))
  }

  return(recoded_df)
}
