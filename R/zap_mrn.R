#' Clean and Validate MRN Column
#'
#' `zap_mrn()` is a convenience function for standardizing and checking the integrity of a medical record number (MRN) column in a dataset. It trims whitespace, pads MRNs to 8 characters with leading zeros, and checks for missing or duplicate values.
#'
#' If duplicate MRNs are found, the user can optionally print a summary of duplicate records to the console or open a detailed view of all duplicated rows in the RStudio `View()` pane.
#'
#' @param data A data frame or tibble that includes a column named `mrn`.
#' @param showdups Logical. If `TRUE`, prints a summary table of duplicate MRNs to the console. This includes the MRN, any columns containing "date", and either the row number or the first column if it's not `mrn`. Default is `FALSE`.
#' @param inspectdups Logical. If `TRUE`, opens a `View()` tab in RStudio showing all duplicate MRNs with all columns, plus any columns containing "date". Includes either the row number or the first column as an identifier. Default is `FALSE`.
#'
#' @return The original data frame (with a cleaned `mrn` column), returned invisibly. Summary information is printed to the console.
#'
#' @examples
#' library(dplyr)
#'
#' # Sample dataset with duplicate and malformed MRNs
#' df <- tibble::tibble(
#'   mrn = c(12345, "6789012", "00001234", NA, "6789012"),
#'   visit_date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10", NA, "2024-01-05")),
#'   name = c("Alice", "Bob", "Charlie", "Diana", "Bob Duplicate")
#' )
#'
#' # Basic usage
#' cleaned <- zap_mrn(df)
#'
#' # Show duplicates in console
#' zap_mrn(df, showdups = TRUE)
#'
#' # Inspect duplicates in View tab
#' zap_mrn(df, inspectdups = TRUE)
#'
#' @export

zap_mrn <- function(data, showdups = FALSE, inspectdups = FALSE) {
  stopifnot(is.data.frame(data))

  if (!"mrn" %in% names(data)) {
    stop("⚠️ Column 'mrn' not found in the dataset.")
  }

  # Clean MRNs
  data <- data |>
    dplyr::mutate(
      mrn = stringr::str_trim(as.character(mrn)),
      mrn = dplyr::if_else(!is.na(mrn) & nchar(mrn) < 8,
                           stringr::str_pad(mrn, 8, pad = "0"),
                           mrn)
    )

  n_total <- nrow(data)
  n_missing <- sum(is.na(data$mrn) | data$mrn == "")
  n_unique <- dplyr::n_distinct(data$mrn)
  n_dupes <- n_total - n_unique

  message("✅ MRNs trimmed and padded to 8 digits.\n")
  message("🔎 MRN Quality Check")
  message(glue::glue("Total Rows:      {n_total}"))
  message(glue::glue("Unique MRNs:     {n_unique}"))
  message(glue::glue("Duplicate MRNs:  {n_dupes}"))
  message("")

  if (n_missing > 0) {
    message(glue::glue("⚠️ {n_missing} missing MRNs found."))
  } else {
    message("✅ No missing MRNs.")
  }

  if (n_dupes > 0) {
    message(glue::glue("❗ {n_dupes} duplicate rows found for MRN."))

    dup_mrns <- data$mrn[duplicated(data$mrn) | duplicated(data$mrn, fromLast = TRUE)]
    dup_rows <- dplyr::filter(data, mrn %in% dup_mrns)

    # Add row number if no natural ID is available
    identifier <- if ("mrn" != names(data)[1]) names(data)[1] else ".row_id"
    if (identifier == ".row_id") {
      dup_rows <- dplyr::mutate(dup_rows, .row_id = dplyr::row_number())
    }

    # showdups: print mrn, date columns, and identifier
    if (showdups) {
      to_print <- dup_rows |>
        dplyr::select(dplyr::any_of(identifier), mrn, dplyr::contains("date")) |> arrange(mrn)
      print(to_print)
    }

    # inspectdups: open in View tab
    if (inspectdups) {
      to_view <- dup_rows |>
        dplyr::select(dplyr::any_of(identifier), mrn, dplyr::contains("date"), dplyr::everything()) |> arrange(mrn)
      View(to_view, title = "Duplicate MRNs")
    }

  } else {
    message("✅ All MRNs are unique.")
  }

  invisible(data)
}
