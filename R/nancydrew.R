nancydrew <- function(data) {
  stopifnot(is.data.frame(data))

  # 1. Variable types summary
  var_types <- sapply(data, function(x) class(x)[1])
  type_levels <- sort(table(var_types))  # alphabetized by default

  message("📊 Variable Types:")

  for (type in sort(names(type_levels))) {
    vars <- names(var_types[var_types == type])
    var_string <- paste(vars, collapse = ", ")
    cat(sprintf("  %-12s: %3d   (%s)\n\n", type, type_levels[[type]], var_string))
  }

  # 2. Factor summary
  factor_vars <- dplyr::select(data, dplyr::where(is.factor))
  if (ncol(factor_vars) > 0) {
    message("")
    codebook_data <- data_codebook(factor_vars, max_values = 10)
    codebook_data |>
      dplyr::select(-ID, -.row_id) |>
      gt::gt() |>
      gt::tab_header(title = "🔎 Factor Variables Summary") |>
      print()
  } else {
    message("ℹ️ No factor variables found.")
  }

  # 3. Character variables summary
  char_vars <- dplyr::select(data, dplyr::where(is.character))
  if (ncol(char_vars) > 0) {
    char_summary <- purrr::map_df(names(char_vars), function(var) {
      vec <- char_vars[[var]]
      tibble::tibble(
        variable = var,
        n_missing = sum(is.na(vec) | vec == ""),
        pct_missing = round(mean(is.na(vec) | vec == "") * 100, 1),
        n_unique = dplyr::n_distinct(vec),
        avg_length = round(mean(nchar(vec), na.rm = TRUE), 1)
      )
    })
    message()
    char_summary |>
      gt::gt() |>
      gt::tab_header(title = "📝 Character Variables Summary") |>
      print()
  } else {
    message("ℹ️ No character variables found.")
  }

  # 4. Date variables summary
  date_vars <- dplyr::select(data, dplyr::where(lubridate::is.Date))
  if (ncol(date_vars) > 0) {
    date_summary <- purrr::map_df(names(date_vars), function(var) {
      vec <- date_vars[[var]]
      vec_clean <- vec[!is.na(vec)]
      tibble::tibble(
        variable = var,
        min_date = min(vec_clean, na.rm = TRUE),
        max_date = max(vec_clean, na.rm = TRUE),
        years = paste(sort(unique(lubridate::year(vec_clean))), collapse = ", ")
      )
    })
    message("")
    print(date_summary |>
            gt::gt() |>
            gt::tab_header(title = "📅 Date Variables Summary"))
  } else {
    message("ℹ️ No date variables found.")
  }

  invisible(NULL)
}

