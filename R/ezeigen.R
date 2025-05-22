#' ezeigen: Summarize eigenvalues, variance explained, and factor reliability
#'
#' Summarizes eigenvalues, SS loadings, variance explained, and includes Cronbach's alpha
#' and McDonald's omega for each factor. Items are assigned to the factor with the highest
#' absolute loading (optionally above a threshold). Requires original data.
#'
#' @param fa_object An object returned by `psych::fa()` or `psych::principal()`.
#' @param data The original data frame used for factor analysis (required for alpha and omega).
#' @param digits Number of decimal places to round values. Default is 2.
#' @param gt Logical. If TRUE, returns a formatted `gt` table. Default is FALSE.
#' @param min_loading Minimum absolute loading required to assign an item to a factor. Default is 0.30.
#' @param print_items Logical. If TRUE, prints which items were assigned to each factor. Default is FALSE.
#'
#' @return A tibble or `gt` table with eigenvalues, variance explained, and reliability estimates.
#' @examples
#' \dontrun{
#' fa_res <- psych::fa(mtcars[, 1:6], nfactors = 2)
#' ezeigen(fa_res, data = mtcars[, 1:6], print_items = TRUE)
#' }
#' @export
ezeigen <- function(fa_object, data = NULL, digits = 2, gt = FALSE,
                    min_loading = 0.30, print_items = FALSE) {
  if (is.null(fa_object$Vaccounted) || is.null(fa_object$e.values)) {
    stop("Input must be a valid psych::fa() or psych::principal() object.")
  }

  eigenvalues <- fa_object$e.values
  SS_loadings <- fa_object$Vaccounted[1, ]
  prop_var <- fa_object$Vaccounted[2, ]
  cum_var <- fa_object$Vaccounted[3, ]
  num_factors <- length(SS_loadings)

  result <- tibble::tibble(
    Factor = paste0("Factor ", seq_len(num_factors)),
    Eigenvalue = round(eigenvalues[1:num_factors], digits),
    SS_Loadings = round(SS_loadings, digits),
    Proportion_Variance = round(prop_var, digits),
    Cumulative_Variance = round(cum_var, digits)
  )

  # Reliability estimates (Cronbach's alpha and McDonald's omega)
  if (!is.null(data)) {
    loadings <- as.data.frame(unclass(fa_object$loadings))
    item_names <- rownames(loadings)

    # Assign each item to its highest loading factor, but only if loading exceeds threshold
    max_abs <- apply(abs(loadings), 1, max)
    max_factors <- apply(abs(loadings), 1, which.max)

    assigned_items <- lapply(seq_len(num_factors), function(f) {
      item_names[max_factors == f & max_abs >= min_loading]
    })

    alpha_vals <- numeric(num_factors)
    omega_vals <- numeric(num_factors)
    n_items <- numeric(num_factors)

    for (i in seq_len(num_factors)) {
      items <- assigned_items[[i]]
      n_items[i] <- length(items)

      if (print_items) {
        message("Factor ", i, " (", length(items), " items): ", paste(items, collapse = ", "))
      }

      if (length(items) >= 2) {
        subdata <- data[, items, drop = FALSE]

        alpha_vals[i] <- tryCatch(
          psych::alpha(subdata)$total$raw_alpha,
          error = function(e) NA_real_
        )

        omega_vals[i] <- tryCatch(
          psych::omega(subdata, nfactors = 1, warnings = FALSE, plot = FALSE)$omega.tot,
          error = function(e) NA_real_
        )
      } else {
        alpha_vals[i] <- NA
        omega_vals[i] <- NA
      }
    }

    result$n_items <- n_items
    result$alpha <- round(alpha_vals, digits)
    result$omega <- round(omega_vals, digits)
  }

  if (gt) {
    return(
      result |>
        gt::gt() |>
        gt::fmt_number(columns = 2:(ncol(result) - 3), decimals = digits) |>
        gt::fmt_number(columns = "n_items", decimals = 0) |>
        gt::fmt_number(columns = c("alpha", "omega"), decimals = digits) |>
        gt::tab_header(title = "Eigen Values and Explained Variance of Factors")
    )
  }

  result
}
